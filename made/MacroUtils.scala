package made

import made.annotation.{name, repeated, AnnotationAggregate, MetaAnnotation}

import scala.annotation.{tailrec, Annotation, StaticAnnotation}
import scala.collection.immutable.List
import scala.quoted.*

// $COVERAGE-OFF$
// like ValueOf but without the implicit search and boxing
inline private[made] def singleValueOf[T <: scala.Singleton]: T = ${ singleValueOfImpl[T] }

private def singleValueOfImpl[T <: scala.Singleton: Type](using quotes: Quotes): Expr[T] =
  import quotes.reflect.*
  val term = TypeRepr.of[T] match
    case ConstantType(c: Constant) => Literal(c)
    case tp: TypeRef if tp <:< TypeRepr.of[Unit] => Literal(UnitConstant())
    case n: TermRef => Ref(n.termSymbol)
    case ts: ThisType => This(ts.classSymbol.get)
    case tp => report.errorAndAbort(s"Unsupported singleton type: ${tp.show}")
  term.asExprOf[T]

extension (comp: Expr.type)
  private[made] def ofOption[A: Type](opt: Option[Expr[A]])(using Quotes): Expr[Option[A]] = opt match
    case Some(expr) => '{ Some($expr) }
    case None => '{ None }

private[made] def stringToType(str: String)(using quotes: Quotes): Type[? <: String] =
  import quotes.reflect.*
  ConstantType(StringConstant(str)).asType.asInstanceOf[Type[? <: String]]

private[made] def typeToString[S <: String: Type](using quotes: Quotes): S =
  import quotes.reflect.*
  TypeRepr.of[S] match
    case ConstantType(StringConstant(str)) => str.asInstanceOf[S]
    case _ => report.errorAndAbort(s"Unsupported singleton type: ${Type.show[S]}")

private[made] def traverseTypes(tpes: Iterable[Type[? <: AnyKind]])(using Quotes): Type[? <: Tuple] =
  val empty: Type[? <: Tuple] = Type.of[EmptyTuple]
  tpes.foldRight(empty):
    case ('[tpe], '[type acc <: Tuple; acc]) => Type.of[tpe *: acc]
    case (_, _) => wontHappen

private[made] def traverseTuple(tpe: Type[? <: Tuple])(using Quotes): List[Type[? <: AnyKind]] = tpe match
  case '[EmptyTuple] => Nil
  case '[t *: ts] => Type.of[t] :: traverseTuple(Type.of[ts])

extension (companion: Expr.type)
  private[made] def ofRefinedTuple(exprs: List[Expr[?]])(using Quotes): Expr[Tuple] = exprs.runtimeChecked match
    case Nil => '{ EmptyTuple }
    case '{ $headExpr: h } :: tail =>
      ofRefinedTuple(tail) match
        case '{ type t <: Tuple; $tailExpr: t } =>
          '{ ${ headExpr.asExprOf[h] } *: ${ tailExpr.asExprOf[t] } }

// Compiler-synthesized accessor for a defaulted parameter (e.g. `op$default$1` for
// `def op(x: Int = 5)`). It carries no `Synthetic`/`Artifact` flag (confirmed via
// `Symbol.flags.show`: only `Flags.Method`), so it can't be filtered out by flags alone.
private[made] val DefaultParamAccessorName = """(.*)\$default\$(\d+)$""".r

private[made] def nameOf[T: Type](using quotes: Quotes): String =
  import quotes.reflect.*
  val tpe = TypeRepr.of[T]
  val termSym = tpe.termSymbol
  if termSym.isNoSymbol then tpe.typeSymbol.name else termSym.name

private[made] def reportOnDuplicates(labels: Seq[(label: String, original: String)])(using Quotes): Unit =
  import quotes.reflect.*
  labels
    .groupMap(_.label)(_.original)
    .foreach: (label, originals) =>
      if originals.sizeIs > 1 then report.error(s"${originals.mkString(", ")} have the same @name: $label")

private[made] class MacroUtils[Q <: Quotes](using val quotes: Q):
  import quotes.reflect.*

  extension (symbol: Symbol)
    /**
     * Symbols whose annotations are considered to "belong" to this symbol: the symbol itself,
     * the matching constructor parameter of its owning class (for case class accessors), and all
     * symbols overridden by this one.
     */
    def annotationSymbols: List[Symbol] =
      metaSymbolsOf(symbol) ::: symbol.allOverriddenSymbols.toList

    /**
     * Annotations on `annotationSymbols` with `AnnotationAggregate` annotations recursively
     * expanded.
     */
    def expandedAnnotations: List[Term] =
      expandAggregates(symbol.annotationSymbols.flatMap(_.annotations))

    def hasAnnotationOf[AT <: Annotation: Type] =
      val at = TypeRepr.of[AT]
      symbol.expandedAnnotations.exists(_.tpe <:< at)

    def hasOrInheritsAnnotationOf[AT <: Annotation: Type] =
      symbol.hasAnnotationOf[AT]

    def getAnnotationOf[AT <: Annotation: Type] =
      val at = TypeRepr.of[AT]
      symbol.expandedAnnotations.find(_.tpe <:< at).map(_.asExprOf[AT])

  def expandAggregates(annots: List[Term]): List[Term] =
    val aggregateTpe = TypeRepr.of[AnnotationAggregate]
    val staticTpe = TypeRepr.of[StaticAnnotation]

    def collectArgs(annot: Term): (List[TypeRepr], List[Term]) =
      def loop(t: Term, vAcc: List[Term]): (List[TypeRepr], List[Term]) = t match
        case Apply(fun, args) => loop(fun, args ++ vAcc)
        case TypeApply(fun, tArgs) =>
          val (_, vs) = loop(fun, vAcc)
          (tArgs.map(_.tpe), vs)
        case Select(New(tpt), _) => (tpt.tpe.typeArgs, vAcc)
        case _ => (Nil, vAcc)
      loop(annot, Nil)

    def substituteRefs(term: Term, valueMap: Map[Symbol, Term]): Term =
      val byName: Map[String, Term] = valueMap.map((sym, t) => sym.name -> t)
      val tr = new TreeMap:
        override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
          case id: Ident if byName.contains(id.name) => byName(id.name)
          case Select(This(_), n) if byName.contains(n) => byName(n)
          case other => super.transformTerm(other)(owner)
      tr.transformTerm(term)(Symbol.spliceOwner)

    def rebuildAnnot(inner: Term, valueMap: Map[Symbol, Term]): Term =
      val annotCls = inner.tpe.typeSymbol
      val ctor = annotCls.primaryConstructor
      val (_, rawArgs) = collectArgs(inner)
      val concreteArgs = rawArgs.map(substituteRefs(_, valueMap))
      val classTpe = annotCls.typeRef
      val newTree: Term = New(Inferred(classTpe))
      val selectedCtor: Term = Select(newTree, ctor)
      val typeArgs = inner.tpe.typeArgs
      val withTypeArgs: Term =
        if typeArgs.isEmpty then selectedCtor
        else TypeApply(selectedCtor, typeArgs.map(t => Inferred(t)))
      Apply(withTypeArgs, concreteArgs)

    def expand(annot: Term): List[Term] =
      if !(annot.tpe <:< aggregateTpe) then List(annot)
      else
        val cls = annot.tpe.typeSymbol
        cls.declaredMethods.find(_.name == "aggregated") match
          case None => List(annot)
          case Some(aggMethod) =>
            val valueParams = cls.primaryConstructor.paramSymss.flatten.filterNot(_.isType)
            val (_, outerValueArgs) = collectArgs(annot)
            val valueMap: Map[Symbol, Term] = valueParams.zip(outerValueArgs).toMap
            val rawInner = aggMethod.annotations.filter(_.tpe <:< staticTpe)
            rawInner.flatMap(inner => expand(rebuildAnnot(inner, valueMap)))

    annots.flatMap(expand)

  def metaTypeOf(symbol: Symbol): Type[? <: Tuple] =
    val userAnnots = symbol.expandedAnnotations.iterator
      .filter(_.tpe <:< TypeRepr.of[MetaAnnotation])
      .map(annot => AnnotatedType(TypeRepr.of[Meta], annot).asType)

    val syntheticAnnot = Option.when(isRepeatedCtorParam(symbol)):
      AnnotatedType(TypeRepr.of[Meta], '{ new repeated }.asTerm).asType
    traverseTypes(userAnnots.concat(syntheticAnnot).toList)

  private def isRepeatedCtorParam(symbol: Symbol): Boolean = symbol.flags.is(Flags.CaseAccessor) &&
    symbol.owner.primaryConstructor.paramSymss.iterator.flatten
      .find(_.name == symbol.name)
      .map(_.tree)
      .collect:
        case ValDef(_, Annotated(_, annot), _) => annot.tpe
      .exists: tpe =>
        tpe <:< TypeRepr.of[scala.annotation.internal.Repeated]

  private def metaSymbolsOf(symbol: Symbol): List[Symbol] =
    val ctorParam = for
      owner <- List(symbol.maybeOwner)
      if !owner.isNoSymbol
      if owner.isClassDef
      ctor = owner.primaryConstructor
      if !ctor.isNoSymbol
      ctorParam <- ctor.paramSymss.iterator.flatten.filterNot(_.isType).find(_.name == symbol.name)
    yield ctorParam
    symbol :: ctorParam

  // `methodMembers` (unlike `declaredMethods`) includes inherited-not-overridden members, but it
  // also pulls in members from universal/framework base types (Any/Object, and the synthetic
  // `Product`/`Equals`/`Enum` surface on case classes and enums: hashCode, equals, ==, canEqual,
  // productArity, productElement, ordinal, etc.). This method keeps only user-declared
  // val/def operations and drops that noise by owner + synthetic/artifact flags.
  extension (tpe: TypeRepr)
    def userDeclaredMembers: List[Symbol] =
      val excluding = Set(
        defn.AnyClass,
        defn.AnyValClass,
        TypeRepr.of[Object].typeSymbol,
        TypeRepr.of[Product].typeSymbol,
        TypeRepr.of[Equals].typeSymbol,
        TypeRepr.of[scala.reflect.Enum].typeSymbol,
      )
      tpe.typeSymbol.fieldMembers.iterator
        .concat(tpe.typeSymbol.methodMembers)
        .distinct
        .filter: m =>
          (m.isDefDef || m.isValDef) && !m.isClassConstructor && !m.flags.is(Flags.Synthetic) &&
            !m.flags.is(Flags.Artifact) && !excluding.contains(m.owner)
        .toList
        .sortBy(_.pos.getOrElse(Position.ofMacroExpansion))

  def labelTypeOf(sym: Symbol, fallback: String): Type[? <: String] =
    val syms = Iterator(sym) ++ sym.allOverriddenSymbols
    val res = syms.find(_.hasAnnotationOf[name]).flatMap(_.getAnnotationOf[name])
    stringToType(res.flatMap(extractNameArg).getOrElse(fallback))

  private def extractNameArg(annot: Expr[name]): Option[String] =
    @tailrec def loop(stack: List[Term]): Option[String] = stack match
      case Nil => None
      case Literal(StringConstant(s)) :: _ => Some(s)
      case Apply(fun, args) :: rest => loop(fun :: args ::: rest)
      case TypeApply(fun, _) :: rest => loop(fun :: rest)
      case _ :: rest => loop(rest)
    loop(annot.asTerm :: Nil)

private[made] given (quotes: Quotes) => Ordering[quotes.reflect.Position] =
  Ordering.by(pos => (pos.sourceFile.path, pos.start, pos.end))
// $COVERAGE-ON$
