package made

import scala.annotation.{implicitNotFound, tailrec}
import scala.quoted.*
import scala.NamedTuple.{AnyNamedTuple, NamedTuple}

/**
 * Mirror for operation-centric types, describing the methods and fields of a type `T`
 * as a tuple of [[DoneOperation]]s.
 *
 * While [[Made]] models a type by its constructor parameters or subtypes, `Done` models
 * it by its members: each `val`, `def`, or field becomes an operation whose input
 * parameters and output type are captured at the type level. This makes `Done` suited to
 * describing services, RPC interfaces, enums with behavior, and other types where the API
 * surface (rather than the data shape) is what matters.
 *
 * @example
 * {{{
 * import made.*
 *
 * trait Service:
 *   def ping(message: String): Boolean
 *   def version: Int
 *
 * val done: Done.Of[Service] = Done.derived[Service]
 * // done type members:
 * //   type Type = Service
 * //   type Label = "Service"
 * //   type Metadata = EmptyTuple
 * //   type Operations = DoneOperation { ... "ping" ... } *: DoneOperation { ... "version" ... } *: EmptyTuple
 * }}}
 *
 * @see [[DoneOperation]]
 * @see [[InputElem]]
 * @see [[Done.derived]]
 */
sealed trait Done:
  /** The mirrored type `T`. */
  type Type

  /** The simple name of `T` (or the override provided by `@name`). */
  type Label <: String

  /**
   * Annotation metadata on `T`, represented as a [[Tuple]] of `Meta @ann` entries. When no
   * `MetaAnnotation` annotations are present, `Metadata = EmptyTuple`. When annotations are
   * present, `Metadata` becomes `(Meta @Ann1, Meta @Ann2, ...)`.
   */
  type Metadata <: Tuple

  /** Tuple of [[DoneOperation]] subtypes, one per field or method of `T`. */
  type Operations <: Tuple

  def operations: Operations

  /** Path-dependent evidence that [[Metadata]] is a tuple of `Meta` entries. */
  given Metadata containsOnly Meta = containsOnly.refl

  /** Path-dependent evidence that [[Operations]] is a tuple of [[DoneOperation]]s. */
  given Operations containsOnly DoneOperation = containsOnly.refl

/**
 * Element representing a single operation (field or method) in a [[Done]] mirror.
 *
 * Each entry in [[Done.Operations]] is a `DoneOperation` describing one member of the
 * mirrored type: its label, metadata, input parameters, and output type. Methods with
 * multiple parameter lists are flattened into a single [[InputElems]] tuple.
 *
 * @see [[Done]]
 * @see [[InputElem]]
 */
sealed trait DoneOperation:
  type Type

  /** The member name (or the override provided by `@name`). */
  type Label <: String

  /**
   * Annotation metadata on the member, represented as a [[Tuple]] of `Meta @ann` entries.
   * `EmptyTuple` when no `MetaAnnotation` annotations are present.
   */
  type Metadata <: Tuple

  /** Tuple of [[InputElem]] subtypes describing the operation's parameters, in declaration order. */
  type InputElems <: Tuple

  /**
   * Per-parameter-list arities, as a tuple of singleton `Int` types. Distinguishes method shapes
   * that [[InputElems]] alone cannot:
   *   - no-parens `def f` / `val`  => `EmptyTuple` (zero param lists)
   *   - empty-parens `def f()`     => `0 *: EmptyTuple` (one empty param list)
   *   - `def f(a)(b, c)`           => `1 *: 2 *: EmptyTuple`
   * [[InputElems]] stays flattened; `ParamLists` records the list boundaries so dispatch can
   * reconstruct them. (Phase 1 representation; full tuple-of-tuples is deferred.)
   */
  type ParamLists <: Tuple

  final type Args = Tuple.Map[InputElems, InputElem.ExtractOf]

  def inputElems: InputElems

  /** Path-dependent evidence that [[Metadata]] is a tuple of `Meta` entries. */
  given Metadata containsOnly Meta = containsOnly.refl

  /** Path-dependent evidence that [[InputElems]] is a tuple of [[InputElem]]s. */
  given InputElems containsOnly InputElem = containsOnly.refl

  /** The enclosing type that declares this operation — equals [[Done.Type]] of the parent [[Done]] mirror. */
  type OuterType

  /** The return type of the operation. */
  type OutputType

  /**
   * Invokes the underlying member on an instance of the enclosing type.
   *
   * Arguments in `args` correspond positionally to [[InputElems]] (multiple parameter
   * lists are flattened). Each argument is unboxed via `asInstanceOf` to its declared
   * parameter type, so supplying a value of the wrong type will fail at runtime.
   *
   * @param outer the instance on which to invoke the member
   * @param args  the positional arguments, in [[InputElems]] order
   */
  def apply(outer: OuterType, args: Args): OutputType

object DoneOperation:
  type Of[T] = DoneOperation { type OuterType = T }
  type WithOutput[O] = DoneOperation { type OutputType = O }
  type WithElems[IE <: Tuple] = DoneOperation { type InputElems = IE }
  type ExtractOf[X /* <: DoneOperation */ ] = X match
    case DoneOperation.Of[t] => t
  type ExtractOutput[Op] = Op match
    case WithOutput[o] => o
  type ExtractInputElems[Op] <: Tuple = Op match
    case WithElems[ie] => ie

  /**
   * Mix-in providing an ergonomic `apply(outer, arg)` shortcut for operations with exactly
   * one input. The self-type refinement requires the mixing class to have
   * `Args = Arg *: EmptyTuple`.
   */
  trait SingleApply extends DoneOperation:
    self: DoneOperation { type Args = Arg *: EmptyTuple } =>
    type Arg

    final def apply(outer: OuterType, arg: Arg): OutputType = this.apply(outer, arg *: EmptyTuple)

  /**
   * Mix-in providing an ergonomic `apply(outer)` shortcut for parameterless operations.
   * The self-type refinement requires the mixing class to have `Args = EmptyTuple`.
   */
  trait EmptyApply extends DoneOperation:
    self: DoneOperation { type Args = EmptyTuple } =>

    final def apply(outer: OuterType): OutputType = this.apply(outer, EmptyTuple)

/**
 * Element representing a single input parameter of a [[DoneOperation]].
 *
 * Carries the parameter's type, label, and annotation metadata.
 *
 * @see [[DoneOperation]]
 * @see [[Done]]
 */
sealed trait InputElem:
  /** The parameter's type. */
  type Type

  /** The parameter's name (or the override provided by `@name`). */
  type Label <: String

  /**
   * Annotation metadata on the parameter, represented as a [[Tuple]] of `Meta @ann` entries.
   * `EmptyTuple` when no `MetaAnnotation` annotations are present. Currently always
   * `EmptyTuple` — parameter annotations are not yet captured by [[Done.derived]].
   */
  type Metadata <: Tuple

  /** Path-dependent evidence that [[Metadata]] is a tuple of `Meta` entries. */
  given Metadata containsOnly Meta = containsOnly.refl

object InputElem:
  type Of[T] = InputElem { type Type = T }
  type LabelOf[l <: String] = InputElem { type Label = l }
  type ExtractOf[X /* <: InputElem */ ] = X match
    case InputElem.Of[t] => t
  type ExtractLabel[E] <: String = E match
    case LabelOf[l] => l
  type OuterOf[T] = InputElem { type OuterType = T }

object Done:
  type Of[T] = Done { type Type = T }

  /**
   * Maps a tuple of [[DoneOperation]]s to the corresponding tuple of handler function types.
   * Each operation with no parameters maps to `() => OutputType`; each operation with
   * parameters maps to `(p1: T1, p2: T2, ...) => OutputType` (a named-tuple function).
   */
  type HandlerOf[Op] = DoneOperation.ExtractInputElems[Op] match
    case EmptyTuple => () => DoneOperation.ExtractOutput[Op]
    case _ =>
      NamedTuple[
        Tuple.Map[DoneOperation.ExtractInputElems[Op], InputElem.ExtractLabel],
        Tuple.Map[DoneOperation.ExtractInputElems[Op], InputElem.ExtractOf],
      ] => DoneOperation.ExtractOutput[Op]

  type HandlersOf[Ops <: Tuple] = Tuple.Map[Ops, HandlerOf]

  /**
   * Type-safe entry point for [[DoneOperation.apply]] — enforces at compile time
   * that `op` was derived from the same mirror (its [[DoneOperation.OuterType]]
   * equals the mirror's [[Done.Type]]).
   */
  extension [T](done: Done.Of[T])
    def invoke(
      op: DoneOperation { type OuterType = T },
      target: T,
      args: op.Args,
    ): op.OutputType = op.apply(target, args)

  /**
   * Derives a [[Done]] mirror for `T` at compile time.
   *
   * Each field member and declared method of `T` becomes a [[DoneOperation]] in the
   * resulting [[Done.Operations]] tuple. Methods with multiple parameter lists are
   * flattened into a single [[DoneOperation.InputElems]] tuple.
   *
   * @see [[Done]]
   * @see [[DoneOperation]]
   * @see [[InputElem]]
   */
  transparent inline given derived[T]: Done.Of[T] = ${ derivedImpl[T] }

  // workaround for https://github.com/scala/scala3/issues/25245
  private sealed trait DoneOperationWorkaround[Outer] extends DoneOperation:
    final type OuterType = Outer

  // $COVERAGE-OFF$
  private def derivedImpl[T: Type](using quotes: Quotes): Expr[Done.Of[T]] =
    import quotes.reflect.*
    val utils = new MacroUtils[quotes.type]
    import utils.*

    val tTpe = TypeRepr.of[T]
    val tSymbol = tTpe.typeSymbol

    type Param = (name: Type[? <: String], tpe: Type[?])

    object extract:
      def unapply(tpe: TypeRepr): (List[Param], Type[? <: AnyKind]) = tpe match
        case MethodType(
              paramNames,
              paramTypes,
              extract(outputParams, outputTpe),
            ) =>
          (
            paramNames.zip(paramTypes).map((name, tpe) => (stringToType(name), tpe.asType)) ++ outputParams,
            outputTpe,
          )
        case other =>
          (Nil, other.asType)

    def invokeExpr[Out: Type](
      member: Symbol,
      memberTpe: TypeRepr,
      outer: Expr[T],
      args: Expr[Tuple],
    ): Expr[Out] =
      def go(tpe: TypeRepr, idx: Int): List[List[Term]] = tpe match
        case MethodType(_, paramTypes, result) =>
          val argTerms = paramTypes.zipWithIndex.map: (pTpe, i) =>
            pTpe.asType match
              case '[t] => '{ $args.productElement(${ Expr(idx + i) }).asInstanceOf[t] }.asTerm
          argTerms :: go(result, idx + paramTypes.size)
        case _ => Nil

      val argLists = go(memberTpe, 0)
      val sel = outer.asTerm.select(member)
      val applied = if argLists.isEmpty then sel else sel.appliedToArgss(argLists)
      applied.asExprOf[Out]

    val operations =
      for
        member <- userDeclaredMembers(tTpe)
        // A method with a default parameter value (`def op(x: Int = 5)`) gets a compiler-synthesized
        // `op$default$1` accessor on the SAME type (even when `op` itself is abstract/deferred)
        if !DefaultParamAccessorName.matches(member.name)
        opTpe = tTpe.memberType(member).widen
        extract(params, outputTpe) = opTpe.runtimeChecked
      yield
        // Term-param symbols of the member, flattened across param lists, aligned positionally
        // with the flattened `params` from `extract`. Empty for vals / no-parens defs.
        val paramSymsFlat = member.paramSymss.flatten.filterNot(_.isType)
        val inputElems = params
          .zip(paramSymsFlat)
          .map: (param, paramSym) =>
            (param.name, param.tpe, metaTypeOf(paramSym)) match
              case (
                    '[type inputLabel <: String; inputLabel],
                    '[inputTpe],
                    '[type paramMeta <: Tuple; paramMeta],
                  ) =>
                '{
                  new InputElem:
                    override type Type = inputTpe
                    override type Label = inputLabel
                    override type Metadata = paramMeta
                }
              case (_, _, _) => wontHappen
        // Per-param-list arities (term params only). `paramSymss` is `Nil` for no-parens
        // defs / vals and `List(Nil)` for an empty-parens `def f()`, so this faithfully
        // distinguishes the two shapes while keeping `InputElems` flattened.
        val paramListSizes: List[Int] = member.paramSymss.map(_.count(!_.isType))
        val paramListsType: Type[? <: Tuple] =
          paramListSizes.foldRight[Type[? <: Tuple]](Type.of[EmptyTuple]):
            case (n, '[type acc <: Tuple; acc]) =>
              ConstantType(IntConstant(n)).asType match
                case '[type nT <: Int; nT] => Type.of[nT *: acc]
                case _ => wontHappen
            case (_, _) => wontHappen
        (
          opTpe.asType,
          labelTypeOf(member, member.name),
          metaTypeOf(member),
          outputTpe,
          Expr.ofRefinedTuple(inputElems),
          paramListsType,
        ).runtimeChecked match
          case (
                '[opTpe],
                '[type opLabel <: String; opLabel],
                '[type opMeta <: Tuple; opMeta],
                '[outputTpe],
                '{ type inputElems <: Tuple; $inputElemsExpr: inputElems },
                '[type paramLists <: Tuple; paramLists],
              ) =>
            params match
              case Nil =>
                '{
                  new DoneOperationWorkaround[T] with DoneOperation.EmptyApply:
                    override type Label = opLabel
                    override type Metadata = opMeta
                    override type InputElems = inputElems
                    override type ParamLists = paramLists
                    override type OutputType = outputTpe

                    override val inputElems: InputElems = $inputElemsExpr
                    override def apply(outer: T, args: Args): outputTpe =
                      ${ invokeExpr[outputTpe](member, opTpe, '{ outer }, '{ args.asInstanceOf[Tuple] }) }
                  : DoneOperation.EmptyApply {
                    type Label = opLabel
                    type Metadata = opMeta
                    type InputElems = inputElems
                    type ParamLists = paramLists
                    type OuterType = T
                    type OutputType = outputTpe
                  }
                }
              case (_, '[argT]) :: Nil =>
                '{
                  new DoneOperationWorkaround[T] with DoneOperation.SingleApply:
                    override type Label = opLabel
                    override type Metadata = opMeta
                    override type InputElems = inputElems
                    override type ParamLists = paramLists
                    override type OutputType = outputTpe
                    override type Arg = argT

                    override val inputElems: InputElems = $inputElemsExpr
                    override def apply(outer: T, args: Args): outputTpe =
                      ${ invokeExpr[outputTpe](member, opTpe, '{ outer }, '{ args.asInstanceOf[Tuple] }) }
                  : DoneOperation.SingleApply {
                    type Label = opLabel
                    type Metadata = opMeta
                    type InputElems = inputElems
                    type ParamLists = paramLists
                    type OuterType = T
                    type OutputType = outputTpe
                    type Arg = argT
                  }
                }
              case _ =>
                '{
                  new DoneOperationWorkaround[T]:
                    override type Label = opLabel
                    override type Metadata = opMeta
                    override type InputElems = inputElems
                    override type ParamLists = paramLists
                    override type OutputType = outputTpe

                    override val inputElems: InputElems = $inputElemsExpr
                    override def apply(outer: T, args: Args): outputTpe =
                      ${ invokeExpr[outputTpe](member, opTpe, '{ outer }, '{ args.asInstanceOf[Tuple] }) }
                  : DoneOperation {
                    type Label = opLabel
                    type Metadata = opMeta
                    type InputElems = inputElems
                    type ParamLists = paramLists
                    type OuterType = T
                    type OutputType = outputTpe
                  }
                }

    (
      labelTypeOf(tSymbol, nameOf[T]),
      metaTypeOf(tSymbol),
      Expr.ofRefinedTuple(operations),
    ).runtimeChecked match
      case (
            '[type label <: String; label],
            '[type meta <: Tuple; meta],
            '{ type operations <: Tuple; $operationsExpr: operations },
          ) =>
        '{
          new Done:
            override type Type = T
            override type Label = label
            override type Metadata = meta
            override type Operations = operations

            override val operations: Operations = $operationsExpr
        }
// $COVERAGE-ON$

@implicitNotFound(
  "Handler tuple mismatch for target operations ${Ops}.\n" + "  Found:    ${Handlers}\n" +
    "  Expected: Done.HandlersOf[${Ops}]\n" + "Each operation needs one handler (in declaration order):\n" +
    "  • parameterless op  → () => OutType\n" +
    "  • parametric op     → (p1: T1, p2: T2, ...) => OutType  (named-tuple argument)",
)
sealed trait ValidHandlers[Ops <: Tuple, Handlers <: Tuple]
object ValidHandlers:
  private val reusable = new ValidHandlers[EmptyTuple, EmptyTuple] {}

  def refl[Ops <: Tuple, Handlers <: Tuple]: ValidHandlers[Ops, Handlers] =
    reusable.asInstanceOf[ValidHandlers[Ops, Handlers]]
  given [Ops <: Tuple, H <: Tuple](using H =:= Done.HandlersOf[Ops]): ValidHandlers[Ops, H] = refl

extension [Handlers <: Tuple](handlers: Handlers)
  /**
   * Synthesizes a `Target` instance from a tuple of per-operation handlers.
   * Each handler must match the corresponding operation in [[Done.Operations]] order:
   * a no-parameter operation expects `() => OutputType`; a parametric operation expects
   * `(p1: T1, ...) => OutputType` (named-tuple function). The [[Done.HandlersOf]] type
   * alias encodes the precise expected type, and is checked at compile time via `=:=`.
   */
  transparent inline def to[Target: Done.Of as done](using ValidHandlers[done.Operations, Handlers]): Target =
    ${ materializeImpl[Target, Handlers]('handlers) }

// $COVERAGE-OFF$
private[made] def materializeImpl[Target: Type, Handlers <: Tuple: Type](
  handlers: Expr[Handlers],
)(using quotes: Quotes,
): Expr[Target] =
  import quotes.reflect.*
  val utils = new MacroUtils[quotes.type]
  import utils.*

  val tTpe = TypeRepr.of[Target]
  val members = tTpe.userDeclaredMembers
  val targetSymbol = tTpe.typeSymbol
  val isTrait = targetSymbol.flags.is(Flags.Trait)

  if !isTrait then
    if targetSymbol.flags.is(Flags.Final) then
      report.errorAndAbort(s"Cannot materialize ${tTpe.show}: only traits or non-final classes are supported")
    val ctorParams = targetSymbol.primaryConstructor.paramSymss.flatten.filterNot(_.isType)
    if ctorParams.nonEmpty then
      report.errorAndAbort(
        s"Cannot materialize ${tTpe.show}: class has constructor parameters (${ctorParams.map(_.name).mkString(", ")}), only parameterless classes or traits are supported",
      )

  val parents =
    if isTrait then List(TypeTree.of[Object], TypeTree.of[Target])
    else List(TypeTree.of[Target])

  def decls(cls: Symbol): List[Symbol] =
    members.map(m => Symbol.newMethod(cls, m.name, tTpe.memberType(m), Flags.Override, Symbol.noSymbol))

  val clsSym = Symbol.newClass(
    Symbol.spliceOwner,
    Symbol.freshName("Materialized"),
    parents.map(_.tpe),
    decls,
    selfType = None,
  )

  def flattenParamTypes(mt: TypeRepr): List[(String, TypeRepr)] = mt match
    case MethodType(pnames, pts, res) => pnames.zip(pts) ++ flattenParamTypes(res)
    case PolyType(_, _, res) => flattenParamTypes(res)
    case _ => Nil

  @tailrec
  def resultOf(mt: TypeRepr): Type[?] = mt match
    case MethodType(_, _, res) => resultOf(res)
    case PolyType(_, _, res) => resultOf(res)
    case other => other.asType

  def iMethod(index: Int) = TypeRepr.of[Handlers].typeSymbol.fieldMember(s"_${index + 1}")

  def methodBody(argss: List[List[Tree]], index: Int, member: Symbol): Expr[?] =
    val flatArgs: List[Term] = argss.flatten.collect { case t: Term => t }
    val memberTpe = tTpe.memberType(member).widen
    val (paramNames, paramTpes) =
      flattenParamTypes(memberTpe).map((name, tpe) => (ConstantType(StringConstant(name)), tpe)).unzip

    val outTpe = resultOf(memberTpe)
    val argsTpe: Option[Type[? <: AnyNamedTuple]] =
      if paramTpes.isEmpty then None
      else
        val tupleN = paramTpes.size match
          case 0 => wontHappen
          case 1 => TypeRepr.of[Tuple1] // for some reason
          case n => defn.TupleClass(n).typeRef
        (tupleN.appliedTo(paramNames).asType, tupleN.appliedTo(paramTpes).asType) match
          case ('[type names <: Tuple; names], '[type types <: Tuple; types]) => Some(Type.of[NamedTuple[names, types]])
          case _ => wontHappen

    val value = handlers.asTerm.select(iMethod(index))
    (argsTpe, outTpe) match
      case (None, '[Unit]) =>
        '{ ${ value.asExprOf[() => Unit] }.apply() }
      case (None, '[o]) =>
        '{ ${ value.asExprOf[() => o] }.apply() }
      case (Some('[a]), '[Unit]) =>
        val argsTuple = '{ ${ Expr.ofTupleFromSeq(flatArgs.map(_.asExpr)) }.asInstanceOf[a] }
        '{ ${ value.asExprOf[a => Unit] }.apply($argsTuple) }
      case (Some('[a]), '[o]) =>
        val argsTuple = '{ ${ Expr.ofTupleFromSeq(flatArgs.map(_.asExpr)) }.asInstanceOf[a] }
        '{ ${ value.asExprOf[a => o] }.apply($argsTuple) }
      case _ => wontHappen

  val methodDefs: List[DefDef] = clsSym.declaredMethods.zipWithIndex.map:
    case (methodSym, index) =>
      DefDef(methodSym, argss => Some(methodBody(argss, index, members(index)).asTerm.changeOwner(methodSym)))

  val clsDef = ClassDef(clsSym, parents, methodDefs)
  val instance = Typed(
    Apply(Select(New(TypeIdent(clsSym)), clsSym.primaryConstructor), Nil),
    TypeTree.of[Target],
  )
  Block(List(clsDef), instance).asExprOf[Target]
// $COVERAGE-ON$
