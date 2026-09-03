package halotukozak.made

import halotukozak.made.annotation.*

import scala.annotation.{implicitNotFound, tailrec}
import scala.deriving.Mirror
import scala.quoted.*
import halotukozak.*
import halotukozak.commons.*

/**
 * Extended mirror for Scala types, providing annotation metadata, element-level detail,
 * and generated member support beyond standard `scala.deriving.Mirror`.
 *
 * A `Made` instance describes the structure of a type `T` at both the type level and runtime.
 * Unlike the standard library `Mirror`, `Made` carries per-element metadata (annotations,
 * default values, labels) and supports `@generated` members that compute derived values.
 *
 * @example
 * {{{
 * import halotukozak.made.*
 *
 * case class User(name: String, age: Int)
 *
 * val mirror = Made.derived[User]
 * // mirror type members:
 * //   type Type = User
 * //   type Label = "User"
 * //   type Metadata = EmptyTuple
 * //   type Elems = MadeFieldElem { ... } *: MadeFieldElem { ... } *: EmptyTuple
 *
 * val (nameFld, ageFld) = mirror.elems
 * val user = mirror.fromUnsafeArray(Array("Alice", 30))
 * }}}
 *
 * @see [[Made.Product]]
 * @see [[Made.Sum]]
 * @see [[Made.Singleton]]
 * @see [[Made.Transparent]]
 * @see [[MadeElem]]
 * @see [[Made.derived]]
 */
@implicitNotFound("No Made could be generated.\nDiagnose any issues by calling Made.derived directly")
sealed trait Made:
  /** Tuple of element types, extracted from [[Elems]] via type-level mapping. */
  final type ElemTypes = Tuple.Map[Elems, MadeElem.ExtractOf]

  /** Tuple of element labels, extracted from [[Elems]] via type-level mapping. */
  final type ElemLabels = Tuple.Map[Elems, MadeElem.ExtractLabel]

  /** The mirrored type `T`. */
  type Type

  /** The simple name of `T` (or the override provided by `@name`). */
  type Label <: String

  /**
   * Annotation metadata on `T`, represented as a [[Tuple]] of `Meta @ann` entries. When no
   * `MetaAnnotation` annotations are present, `Metadata = EmptyTuple`. When annotations are
   * present, `Metadata` becomes `(Meta @Ann1, Meta @Ann2, ...)`. Query via [[hasAnnotation]]
   * and [[getAnnotation]].
   */
  type Metadata <: Tuple

  /** Tuple of [[MadeElem]] subtypes representing constructor fields (for products) or subtypes (for sums). */
  type Elems <: Tuple
  def elems: Elems

  /** Tuple of [[GeneratedMadeElem]] for members annotated with `@generated`. */
  type GeneratedElems <: Tuple
  def generatedElems: GeneratedElems

  /** The type of `T`'s companion object, or `NotExists` if `T` has no companion. */
  type Companion <: AnyRef | NotExists

  /** The companion object of `T`, or `NotExists` if `T` has no companion (e.g. singletons). */
  val companion: Companion

  /**
   * Path-dependent evidence: the mirror's [[Elems]] tuple is guaranteed by the deriver to be
   * a tuple of [[MadeElem]]s. Available wherever this `Made` instance is in scope so plural
   * extensions like `made.elems.hasAnnotations[A]` summon evidence without explicit imports.
   */
  inline given Elems containsOnly MadeElem = containsOnly.refl

  /**
   * Path-dependent evidence: the mirror's [[Metadata]] tuple is a tuple of `Meta` (or
   * `Meta @ann`) entries. Mirrors the guarantee for `hasAnnotation[A]` / `getAnnotation[A]`.
   */
  inline given Metadata containsOnly Meta = containsOnly.refl

  /**
   * Path-dependent evidence: the mirror's [[GeneratedElems]] tuple is a tuple of
   * [[GeneratedMadeElem]]s. Enables `made.generatedElems.hasAnnotations[A]` analogous to
   * `made.elems.hasAnnotations[A]`.
   */
  inline given GeneratedElems containsOnly GeneratedMadeElem = containsOnly.refl

  /** [[ElemLabels]] is all `String`s; path-dependent, so it stays in scope for the `ElemLabels` alias. */
  inline given ElemLabels containsOnly String = containsOnly.refl

/**
 * Base type for elements within a [[Made.Elems]] tuple.
 *
 * Each element in the [[Made.Elems]] tuple is a subtype of `MadeElem`,
 * carrying the element's type, label, and annotation metadata. The concrete
 * subtype depends on the mirror kind:
 *
 *  - [[MadeFieldElem]] - constructor parameters in product mirrors
 *  - [[MadeSubElem]] - non-singleton subtypes in sum mirrors
 *  - [[MadeSubSingletonElem]] - singleton subtypes in sum mirrors
 *  - [[GeneratedMadeElem]] - `@generated` members (in `GeneratedElems`)
 *
 * @example
 * {{{
 * import halotukozak.made.*
 *
 * case class User(name: String, age: Int)
 *
 * val mirror = Made.derived[User]
 * val (nameFld, ageFld) = mirror.elems
 * // nameFld: MadeFieldElem { type Type = String; type Label = "name" }
 * // ageFld:  MadeFieldElem { type Type = Int;    type Label = "age"  }
 * }}}
 *
 * @see [[MadeFieldElem]]
 * @see [[MadeSubElem]]
 * @see [[MadeSubSingletonElem]]
 * @see [[GeneratedMadeElem]]
 */
sealed trait MadeElem:
  /** The element's type (field type or subtype). */
  type Type

  /** The element's label (field name or subtype name, or the override provided by `@name`). */
  type Label <: String

  /**
   * Annotation metadata on this element, represented as a [[Tuple]] of `Meta @ann` entries.
   * When no `MetaAnnotation` annotations are present, `Metadata = EmptyTuple`. When annotations
   * are present, `Metadata` becomes `(Meta @Ann1, Meta @Ann2, ...)`. Query via [[hasAnnotation]]
   * and [[getAnnotation]].
   */
  type Metadata <: Tuple

  /** Path-dependent evidence that this element's [[Metadata]] is a tuple of `Meta` entries. */
  inline given Metadata containsOnly Meta = containsOnly.refl

/**
 * Element representing a constructor parameter in a product type mirror.
 *
 * Each entry in [[Made.Product.Elems]] tuple is a `MadeFieldElem`,
 * providing the field's type, label, metadata, and default value.
 *
 * @see [[MadeElem]]
 * @see [[MadeSubElem]]
 * @see [[GeneratedMadeElem]]
 * @see [[Made.Product]]
 */
sealed trait MadeFieldElem extends MadeElem:
  /** The type that declares this field (the outer/owning type). */
  type OuterType

  /** Reads this field's value from an instance of the declaring type. */
  def apply(outer: OuterType): Type

  /**
   * Resolves a default value for this field using the following priority chain (first match wins):
   *
   *  1. `@whenAbsent(value)` - explicit default from annotation (highest priority)
   *  2. `@optionalParam` - uses `Default[T]` for option-like types
   *  3. Constructor default - the Scala-level default parameter value
   *  4. `NotExists` - no default available
   *
   * @return the default value if available, `NotExists` otherwise
   */
  def default: Type | NotExists

object MadeFieldElem:
  type Of[T] = MadeFieldElem { type Type = T }
  type OuterOf[Outer] = MadeFieldElem { type OuterType = Outer }

/**
 * Element representing a non-singleton subtype in a sum type mirror.
 *
 * Used in [[Made.Sum.Elems]] for subtypes that are not
 * singleton types (e.g., case classes with parameters). For singleton
 * subtypes (case objects, parameterless enum cases), see
 * [[MadeSubSingletonElem]].
 *
 * @see [[MadeElem]]
 * @see [[MadeFieldElem]]
 * @see [[MadeSubSingletonElem]]
 * @see [[Made.Sum]]
 */
sealed trait MadeSubElem extends MadeElem

object MadeSubElem:
  type Of[T] = MadeSubElem { type Type = T }

// workaround for https://github.com/scala/scala3/issues/25245
private sealed trait MadeSubElemWorkaround[T, L <: String] extends MadeSubElem:
  final type Type = T
  final type Label = L

/**
 * Element representing a singleton subtype in a sum type mirror.
 *
 * Extends [[MadeSubElem]]. Used in [[Made.Sum.Elems]] for
 * case objects and parameterless enum cases. Provides access to the
 * singleton instance via the [[value]] method.
 *
 * @see [[MadeSubElem]]
 * @see [[Made.Sum]]
 * @see [[Made.Singleton]]
 */
sealed trait MadeSubSingletonElem extends MadeSubElem:
  /** Returns the singleton instance. */
  def value: Type

object MadeSubSingletonElem:
  type Of[T] = MadeSubSingletonElem { type Type = T }

// workaround for https://github.com/scala/scala3/issues/25245
private sealed trait MadeSubSingletonElemWorkaround[T, L <: String] extends MadeSubSingletonElem:
  final type Type = T
  final type Label = L

/**
 * Element representing a [[generated]] val or def.
 *
 * Extends [[MadeFieldElem]]. Lives in [[Made.GeneratedElems]] tuple
 * (separate from [[Made.Elems]]). A generated element computes a derived
 * value from an instance of the outer type.
 *
 * @see [[MadeFieldElem]]
 * @see [[MadeElem]]
 * @see [[halotukozak.made.annotation.generated]]
 */
sealed trait GeneratedMadeElem extends MadeFieldElem:
  /** Always `NotExists`; generated members have no constructor defaults. */
  final def default: NotExists = NotExists

object GeneratedMadeElem:
  type Of[T] = GeneratedMadeElem { type Type = T }
  type OuterOf[Outer] = GeneratedMadeElem { type OuterType = Outer }

object MadeElem:
  type Of[T] = MadeElem { type Type = T }
  type LabelOf[l <: String] = MadeElem { type Label = l }
  type MetaOf[m <: Tuple] = MadeElem { type Metadata = m }

  type ExtractOf[M /* <: MadeElem */ ] = M match
    case Of[t] => t

  type ExtractLabel[M /* <: MadeElem */ ] <: String = M match
    case LabelOf[label] => label

  type ExtractMeta[M /* <: MadeElem */ ] <: Tuple = M match
    case MetaOf[meta] => meta

  /** `ExtractLabel <: String`, so any `Tuple.Map[Es, ExtractLabel]` is all `String`s (e.g. generated-elem labels). */
  inline given [Es <: Tuple] => (Tuple.Map[Es, ExtractLabel] containsOnly String) = containsOnly.refl

object Made:
  type Of[T] = Made { type Type = T }
  type ProductOf[T] = Made.Product { type Type = T }
  type SumOf[T] = Made.Sum { type Type = T }
  type SingletonOf[T] = Made.Singleton { type Type = T }
  type TransparentOf[T] = Made.Transparent { type Type = T; }

  type LabelOf[l <: String] = MadeElem { type Label = l }
  type MetaOf[m <: Tuple] = MadeElem { type Metadata = m }

  type ExtractOf[M /* <: MadeElem */ ] = M match
    case Of[t] => t

  type ExtractLabel[M /* <: MadeElem */ ] <: String = M match
    case LabelOf[label] => label

  type ExtractMeta[M /* <: MadeElem */ ] <: Tuple = M match
    case MetaOf[meta] => meta

  /**
   * Derives a [[Made]] mirror for `T` at compile time.
   *
   * The concrete subtype of the returned mirror is determined by the
   * following derivation priority (first match wins):
   *
   *  1. [[Singleton]] - `T` is an object, `Unit`, or a singleton type
   *  2. [[Transparent]] - `T` is annotated with `@transparent`
   *     (must have exactly one constructor field; `@generated` members
   *     are not allowed)
   *  3. [[Product]] - `T` is a value class (extends `AnyVal`)
   *  4. [[Product]] - `T` has a `Mirror.ProductOf[T]` (case classes)
   *  5. [[Sum]] - `T` has a `Mirror.SumOf[T]` (sealed traits, enums)
   *
   * The return type is `Made.Of[T]` but the actual runtime type is the
   * more specific subtype listed above.
   *
   * @see [[Made.Product]]
   * @see [[Made.Sum]]
   * @see [[Made.Singleton]]
   * @see [[Made.Transparent]]
   */
  transparent inline given derived[T]: Of[T] = ${ derivedImpl[T] }

  // $COVERAGE-OFF$
  private def derivedImpl[T: Type](using quotes: Quotes): Expr[Made.Of[T]] =
    import quotes.reflect.*

    // dealiasKeepOpaques unfolds transparent aliases (e.g. `type AliasFoo = Foo`) so that
    // the deriver sees the underlying case class while preserving opaque boundaries.
    val tTpe = TypeRepr.of[T].dealiasKeepOpaques
    val tSymbol = tTpe.typeSymbol
    val tCompanion = tSymbol.companionModule

    val generatedElems = for
      member <- (tSymbol.fieldMembers ++ tSymbol.methodMembers).distinct.sortBy(_.pos)
      if member.hasOrInheritsAnnotationOf[generated]
      _ = if !(member.isValDef || member.isDefDef) then
        report.errorAndAbort(
          "@generated can only be applied to vals and defs.",
          member.pos.getOrElse(tSymbol.pos.getOrElse(Position.ofMacroExpansion)),
        )
      _ = member.paramSymss match
        case Nil => // no parameters, it's a val or a def without parameters
        case List(Nil) => // a def with empty parameter list
        case _ =>
          report.errorAndAbort(
            s"@generated cannot be applied to methods with parameters: ${member.name}",
            member.pos.getOrElse(tSymbol.pos.getOrElse(Position.ofMacroExpansion)),
          )
    yield
      val elemTpe = tTpe.memberType(member).widen

      (elemTpe.asType, labelTypeOf(member, member.name), metaTypeOf(member)).runtimeChecked match
        case ('[elemTpe], '[type elemLabel <: String; elemLabel], '[type meta <: Tuple; meta]) =>
          '{
            new GeneratedFieldElemImpl[T, elemTpe](outer => ${ '{ outer }.asTerm.select(member).asExprOf[elemTpe] })
              .asInstanceOf[
                GeneratedMadeElem {
                  type Type = elemTpe
                  type Label = elemLabel
                  type Metadata = meta
                  type OuterType = T
                },
              ]
          }

    def singleCaseFieldOf(symbol: Symbol): Symbol = symbol.caseFields match
      case field :: Nil => field
      case _ => report.errorAndAbort(s"Expected a single case field for ${symbol.name}")

    def madeFieldOf(field: Symbol): Expr[MadeFieldElem] =
      (field.termRef.widen.asType, labelTypeOf(field, field.name), metaTypeOf(field)).runtimeChecked match
        case ('[fieldType], '[type elemLabel <: String; elemLabel], '[type fieldMeta <: Tuple; fieldMeta]) =>
          '{
            new FieldElemImpl[T, fieldType](
              outer => ${ '{ outer }.asTerm.select(field).asExprOf[fieldType] },
              ${ defaultOf[fieldType](0, field) },
            ).asInstanceOf[
              MadeFieldElem {
                type Type = fieldType
                type Label = elemLabel
                type Metadata = fieldMeta
                type OuterType = T
              },
            ]
          }

    def defaultOf[E: Type](index: Int, symbol: Symbol): Expr[E | NotExists] =
      def fromWhenAbsent = symbol
        .getAnnotationOf[whenAbsent[?]]
        .map:
          case '{ `whenAbsent`($value: E) } => value
          case '{ `whenAbsent`($_ : e) } =>
            report.error(s"whenAbsent should have value with type ${Type.show[e]}")
            '{ ??? }

      def fromOptionalParam = Option.when(symbol.hasAnnotationOf[optionalParam]) {
        Expr.summon[Default[E]] match
          case Some(impl) => '{ $impl() }
          case None =>
            report.error(s"optionalParam should be used only for types with Default defined")
            '{ ??? }
      }
      def fromDefaultValue =
        tCompanion.methodMembers.collectFirst:
          case m if m.name.startsWith("$lessinit$greater$default$" + (index + 1)) =>
            val ref = Ref(m)
            val applied = tTpe.typeArgs match
              case Nil => ref
              case args => ref.appliedToTypes(args)
            applied.asExprOf[E]

      fromWhenAbsent.orElse(fromOptionalParam).orElse(fromDefaultValue).getOrElse('{ NotExists })

    def newTFrom(args: List[Expr[?]]): Expr[T] =
      New(TypeTree.of[T])
        .select(tSymbol.primaryConstructor)
        .appliedToArgs(args.map(_.asTerm))
        .asExprOf[T]

    (
      metaTypeOf(tSymbol),
      labelTypeOf(tSymbol, nameOf[T]),
      Expr.ofRefinedTupleFixed(generatedElems.toList),
      // Ascribed to the precise singleton type: `exists`/`notExists` are `inline match`-based and
      // need the scrutinee's static type to be exactly `NotExists.type`, not the broader sealed trait.
      if tCompanion.isNoSymbol then '{ NotExists: NotExists.type } else Ref(tCompanion).asExprOf[AnyRef],
    ).runtimeChecked match
      case (
            '[type meta <: Tuple; meta],
            '[type label <: String; label],
            '{ type generatedElems <: Tuple; $generatedElemsExpr: generatedElems },
            '{ type companion <: AnyRef | NotExists; $companionExpr: companion },
          ) =>
        def deriveSingleton = Option.when(tTpe.isSingleton || tTpe <:< TypeRepr.of[Unit]) {
          Type.of[T] match
            case '[type s <: scala.Singleton; s] =>
              '{
                new MadeSingletonImpl(singleValueOf[s], $generatedElemsExpr)
                  .asInstanceOf[
                    Made.SingletonOf[T] {
                      type Label = label
                      type Metadata = meta
                      type GeneratedElems = generatedElems
                      type Companion = NotExists.type
                    },
                  ]
              }
            case '[Unit] =>
              '{
                new MadeSingletonImpl((), $generatedElemsExpr)
                  .asInstanceOf[
                    Made.SingletonOf[T] {
                      type Label = label
                      type Metadata = meta
                      type GeneratedElems = generatedElems
                      type Companion = NotExists.type
                    },
                  ]
              }
        }

        def deriveTransparent = Option.when(tSymbol.hasAnnotation(TypeRepr.of[transparent].typeSymbol)) {
          if generatedElems.nonEmpty then
            report.errorAndAbort(
              "@generated members are not supported in transparent mirrors",
              tSymbol.pos.getOrElse(Position.ofMacroExpansion),
            )

          madeFieldOf(singleCaseFieldOf(tSymbol)) match
            case '{
                  type fieldType
                  type madeFieldElem <: MadeFieldElem.Of[fieldType]
                  $madeFieldExpr: madeFieldElem
                } =>
              '{
                val tw = TransparentWrapping.derived[fieldType, T]
                new MadeTransparentImpl($madeFieldExpr *: EmptyTuple, tw.unwrap, tw.wrap, $companionExpr)
                  .asInstanceOf[
                    Made.TransparentOf[T] {
                      type Label = label
                      type ElemType = fieldType
                      type Metadata = meta
                      type Elems = madeFieldElem *: EmptyTuple
                      type Companion = companion
                    },
                  ]
              }
        }

        def deriveValueClass = Option.when(tTpe <:< TypeRepr.of[AnyVal]) {
          madeFieldOf(singleCaseFieldOf(tSymbol)) match
            case '{
                  type fieldType
                  type madeFieldElem <: MadeFieldElem.Of[fieldType]
                  $madeFieldExpr: madeFieldElem
                } =>
              '{
                new MadeProductImpl(
                  $madeFieldExpr *: EmptyTuple,
                  product => ${ newTFrom(List('{ product(0).asInstanceOf[fieldType] })) },
                  elemsT => ${ newTFrom(List('{ elemsT.productElement(0).asInstanceOf[fieldType] })) },
                  $generatedElemsExpr,
                  $companionExpr,
                ).asInstanceOf[
                  Made.ProductOf[T] {
                    type Label = label
                    type Metadata = meta
                    type Elems = madeFieldElem *: EmptyTuple
                    type GeneratedElems = generatedElems
                    type Companion = companion
                  },
                ]
              }
        }

        def deriveProduct = Expr.summon[Mirror.ProductOf[T]].map {
          case '{
                type mirroredElemTypes <: Tuple
                type mirroredElemLabels <: Tuple

                $m: Mirror.ProductOf[T] {
                  type MirroredElemTypes = mirroredElemTypes
                  type MirroredElemLabels = mirroredElemLabels
                }
              } =>

            val hasFBound = tSymbol.tree match
              case ClassDef(_, constructor, _, _, _) =>
                @tailrec def containsTypeRef(stack: List[TypeRepr], sym: Symbol): Boolean = stack match
                  case Nil => false
                  case head :: _ if head.typeSymbol == sym => true
                  case AppliedType(tycon, args) :: rest => containsTypeRef(tycon :: args ::: rest, sym)
                  case Refinement(parent, _, refined) :: rest => containsTypeRef(parent :: refined :: rest, sym)
                  case AnnotatedType(inner, _) :: rest => containsTypeRef(inner :: rest, sym)
                  case TypeBounds(lo, hi) :: rest => containsTypeRef(lo :: hi :: rest, sym)
                  case AndType(l, r) :: rest => containsTypeRef(l :: r :: rest, sym)
                  case OrType(l, r) :: rest => containsTypeRef(l :: r :: rest, sym)
                  case MatchType(bound, scrutinee, cases) :: rest =>
                    containsTypeRef(bound :: scrutinee :: cases ::: rest, sym)
                  case (tl: TypeLambda) :: rest => containsTypeRef(tl.paramBounds ::: tl.resType :: rest, sym)
                  case _ :: rest => containsTypeRef(rest, sym)

                constructor.leadingTypeParams.exists:
                  case tp @ TypeDef(_, TypeBoundsTree(lo, hi)) => containsTypeRef(hi.tpe :: Nil, tp.symbol)
                  case tp @ TypeDef(_, tt: TypeTree) => containsTypeRef(tt.tpe :: Nil, tp.symbol)
                  case _ => false
              case _ => false

            val elemTypesList = traverseTupleType(Type.of[mirroredElemTypes])
            val elemLabelsList = traverseTupleType(Type.of[mirroredElemLabels])

            val (exprs, names) = if tSymbol.caseFields.sizeIs == elemTypesList.size then
              elemTypesList
                .lazyZip(elemLabelsList)
                .lazyZip(tSymbol.caseFields)
                .zipWithIndex
                .foldLeft((Vector.empty[Expr[?]], Vector.empty[(label: String, original: String)])):
                  case (
                        (exprs, names),
                        (('[fieldTpe], '[type mirrorLabel <: String; mirrorLabel], fieldSymbol), index),
                      ) =>
                    if hasFBound && TypeRepr.of[fieldTpe] =:= TypeRepr.of[Nothing] then
                      report.warning(
                        s"Field '${fieldSymbol.name}' of F-bounded case class ${tSymbol.fullName} resolves " +
                          "to Nothing at this derivation site. The compiler-synthesized " +
                          "Mirror.Product.fromProduct on F-bounded case classes emits `.asInstanceOf[Nothing]` " +
                          "regardless of type-parameter substitution and will throw ClassCastException at " +
                          "runtime when reading. See scala/scala3#26132.",
                        fieldSymbol.pos.getOrElse(tSymbol.pos.getOrElse(Position.ofMacroExpansion)),
                      )
                    (labelTypeOf(fieldSymbol, fieldSymbol.name), metaTypeOf(fieldSymbol)).runtimeChecked match
                      case ('[type elemLabel <: String; elemLabel], '[type meta <: Tuple; meta]) =>
                        val expr = '{
                          new FieldElemImpl[T, fieldTpe](
                            outer => ${ '{ outer }.asTerm.select(fieldSymbol).asExprOf[fieldTpe] },
                            ${ defaultOf[fieldTpe](index, fieldSymbol) },
                          ).asInstanceOf[
                            MadeFieldElem {
                              type Type = fieldTpe
                              type Label = elemLabel
                              type Metadata = meta
                              type OuterType = T
                            },
                          ]
                        }
                        (exprs :+ expr, names :+ (typeToString[elemLabel], fieldSymbol.name))
                  case _ => wontHappen
            else
              // named tuples
              elemTypesList
                .lazyZip(elemLabelsList)
                .zipWithIndex
                .foldLeft((Vector.empty[Expr[?]], Vector.empty[(label: String, original: String)])):
                  case ((exprs, names), (('[fieldTpe], '[type mirrorLabel <: String; mirrorLabel]), index)) =>
                    val expr = '{
                      new FieldElemImpl[T, fieldTpe](
                        outer =>
                          outer.asInstanceOf[scala.Product].productElement(${ Expr(index) }).asInstanceOf[fieldTpe],
                        NotExists,
                      ).asInstanceOf[
                        MadeFieldElem {
                          type Type = fieldTpe
                          type Label = mirrorLabel
                          type Metadata = EmptyTuple
                          type OuterType = T
                        },
                      ]
                    }
                    (exprs :+ expr, names :+ (typeToString[mirrorLabel], typeToString[mirrorLabel]))
                  case _ => wontHappen

            reportOnDuplicates(names)

            Expr.ofRefinedTupleFixed(exprs.toList) match
              case '{ type mirroredElems <: Tuple; $mirroredElemsExpr: mirroredElems } =>
                '{
                  new MadeProductImpl(
                    $mirroredElemsExpr,
                    product => $m.fromProduct(Tuple.fromArray(product)),
                    elemsT => $m.fromProduct(elemsT),
                    $generatedElemsExpr,
                    $companionExpr,
                  ).asInstanceOf[
                    Made.ProductOf[T] {
                      type Label = label
                      type Metadata = meta
                      type Elems = mirroredElems
                      type GeneratedElems = generatedElems
                      type Companion = companion
                    },
                  ]
                }
        }

        def deriveSum = Expr.summon[Mirror.SumOf[T]].map {
          case '{
                type mirroredElemTypes <: Tuple

                $m: Mirror.SumOf[T] {
                  type MirroredElemTypes = mirroredElemTypes
                }
              } =>

            val (exprs, names) = traverseTupleType(Type.of[mirroredElemTypes])
              .foldLeft((Vector.empty[Expr[?]], Vector.empty[(label: String, original: String)])):
                case ((exprs, names), '[subType]) =>
                  val subType = TypeRepr.of[subType]
                  val subSymbol = if subType.termSymbol.isNoSymbol then subType.typeSymbol else subType.termSymbol

                  (labelTypeOf(subSymbol, subSymbol.name), metaTypeOf(subSymbol)).runtimeChecked match
                    case ('[type elemLabel <: String; elemLabel], '[type meta <: Tuple; meta]) =>
                      val expr = Type.of[subType] match
                        case '[type s <: scala.Singleton; s] =>
                          '{
                            new SubSingletonElemImpl[s](singleValueOf[s])
                              .asInstanceOf[
                                MadeSubSingletonElem { type Type = s; type Label = elemLabel; type Metadata = meta },
                              ]
                          }
                        case '[s] =>
                          '{
                            SubElemImpl
                              .asInstanceOf[
                                MadeSubElem { type Type = subType; type Label = elemLabel; type Metadata = meta },
                              ]
                          }
                      (exprs :+ expr, names :+ (typeToString[elemLabel], subSymbol.name))
                case _ => wontHappen

            reportOnDuplicates(names)

            Expr.ofRefinedTupleFixed(exprs.toList) match
              case '{ type mirroredElems <: Tuple; $mirroredElemsExpr: mirroredElems } =>
                '{
                  new MadeSumImpl(
                    $mirroredElemsExpr,
                    $m.ordinal,
                    $generatedElemsExpr,
                    $companionExpr,
                  ).asInstanceOf[
                    Made.SumOf[T] {
                      type Label = label
                      type Metadata = meta
                      type Elems = mirroredElems
                      type GeneratedElems = generatedElems
                      type Companion = companion
                    },
                  ]
                }
              case '{ $_ : x } => report.errorAndAbort(s"Unexpected Mirror type: ${Type.show[x]}")

          case x => report.errorAndAbort(s"Unexpected Mirror type: ${x.show}")
        }

        deriveSingleton
          .orElse(deriveTransparent)
          .orElse(deriveValueClass)
          .orElse(deriveProduct)
          .orElse(deriveSum)
          .getOrElse:
            report.errorAndAbort(s"Unsupported Mirror type for ${tTpe.show}")
  // $COVERAGE-ON$

  /**
   * Mirror for product types (case classes and value classes).
   *
   * Produced by [[Made.derived]] when `T` is a case class, a zero-field
   * case class, or a value class (extends `AnyVal`).
   *
   * [[Elems]] is a tuple of [[MadeFieldElem]] representing each
   * constructor parameter. [[GeneratedElems]] is a tuple of
   * [[GeneratedMadeElem]] for any `@generated` members.
   *
   * @see [[Made]]
   * @see [[Made.Sum]]
   * @see [[Made.Singleton]]
   * @see [[Made.Transparent]]
   * @see [[MadeFieldElem]]
   * @see [[GeneratedMadeElem]]
   */
  sealed trait Product extends Made:
    /** Constructs an instance of `Type` from an untyped array of field values. */
    def fromUnsafeArray(product: Array[Any]): Type

    /** Constructs an instance of `Type` from a typed tuple of field values. */
    def fromTuple(elems: ElemTypes): Type

    /** A product's [[Elems]] are all [[MadeFieldElem]]s; refines the base `containsOnly MadeElem`. */
    inline given Elems containsOnly MadeFieldElem = containsOnly.refl

  // workaround for https://github.com/scala/scala3/issues/25245
  private sealed trait ProductWorkaround[T] extends Made.Product:
    final type Type = T

  /**
   * Mirror for sum types (sealed traits and enums).
   *
   * Produced by [[Made.derived]] when `T` is a sealed trait or enum.
   *
   * [[Elems]] is a tuple of [[MadeSubElem]] and
   * [[MadeSubSingletonElem]] representing the subtypes.
   *
   * @see [[Made]]
   * @see [[Made.Product]]
   * @see [[Made.Singleton]]
   * @see [[MadeSubElem]]
   * @see [[MadeSubSingletonElem]]
   */
  sealed trait Sum extends Made:
    /** Returns the zero-based index of `value`'s runtime subtype within [[Elems]]. */
    def ordinal(value: Type): Int

    /** A sum's [[Elems]] are all [[MadeSubElem]]s; refines the base `containsOnly MadeElem`. */
    inline given Elems containsOnly MadeSubElem = containsOnly.refl

  // workaround for https://github.com/scala/scala3/issues/25245
  private sealed trait SumWorkaround[T] extends Made.Sum:
    final type Type = T

  /**
   * Mirror for singleton types (objects and Unit).
   *
   * Produced by [[Made.derived]] when `T` is an object, `Unit`, or a
   * singleton type.
   *
   * [[Elems]] is fixed to `EmptyTuple` since singletons have no
   * elements. The singleton instance is available via `value`.
   *
   * @see [[Made]]
   * @see [[Made.Sum]]
   * @see [[Made.Product]]
   */
  sealed trait Singleton extends Made:
    final type Elems = EmptyTuple

    /** Returns the singleton instance. */
    def value: Type
    final def elems: Elems = EmptyTuple

  // workaround for https://github.com/scala/scala3/issues/25245
  private sealed trait SingletonWorkaround[T, L <: String] extends Made.Singleton:
    final type Type = T
    final type Label = L

  /**
   * Mirror for transparent wrapper types (single-field case classes
   * annotated with `@transparent`).
   *
   * Produced by [[Made.derived]] when `T` is a case class annotated
   * with `@transparent` having exactly one constructor field.
   * `@generated` members are not supported on transparent types and
   * will cause a compile error.
   *
   * @see [[Made]]
   * @see [[Made.Product]]
   * @see [[TransparentWrapping]]
   */
  sealed trait Transparent extends Made:
    /** `@generated` members are not supported on transparent types. */
    final type GeneratedElems = EmptyTuple

    /** The single wrapped element's type. */
    type ElemType
    type Elems <: MadeElem.Of[ElemType] *: EmptyTuple

    /** Extracts the wrapped value. */
    def unwrap(value: Type): ElemType

    /** Wraps a value into the transparent type. */
    def wrap(value: ElemType): Type

    final def generatedElems: GeneratedElems = EmptyTuple

    /** A transparent type's single [[Elems]] entry is a [[MadeFieldElem]]; refines `containsOnly MadeElem`. */
    inline given Elems containsOnly MadeFieldElem = containsOnly.refl

private final class FieldElemImpl[Outer, Elem](getter: Outer => Elem, elemDefault: Elem | NotExists)
  extends MadeFieldElem:
  type OuterType = Outer
  type Type = Elem
  def apply(outer: Outer): Elem = getter(outer)
  def default: Elem | NotExists = elemDefault

private object SubElemImpl extends MadeSubElem

private final class SubSingletonElemImpl[S](val value: S) extends MadeSubSingletonElem:
  type Type = S

private final class GeneratedFieldElemImpl[Outer, Elem](getter: Outer => Elem) extends GeneratedMadeElem:
  type OuterType = Outer
  type Type = Elem
  def apply(outer: Outer): Elem = getter(outer)

private final class MadeProductImpl[T, E <: Tuple, G <: Tuple, C <: AnyRef | NotExists](
  val elems: E,
  fromArrayFn: Array[Any] => T,
  fromTupleFn: Tuple => T,
  val generatedElems: G,
  val companion: C,
) extends Made.Product:
  type Type = T
  type Elems = E
  type GeneratedElems = G
  type Companion = C
  def fromUnsafeArray(product: Array[Any]): T = fromArrayFn(product)
  def fromTuple(elems: ElemTypes): T = fromTupleFn(elems)

private final class MadeSumImpl[T, E <: Tuple, G <: Tuple, C <: AnyRef | NotExists](
  val elems: E,
  ordinalFn: T => Int,
  val generatedElems: G,
  val companion: C,
) extends Made.Sum:
  type Type = T
  type Elems = E
  type GeneratedElems = G
  type Companion = C
  def ordinal(value: T): Int = ordinalFn(value)

private final class MadeSingletonImpl[S, G <: Tuple](val value: S, val generatedElems: G) extends Made.Singleton:
  type Type = S
  type GeneratedElems = G
  type Companion = NotExists.type
  val companion: NotExists.type = NotExists

private final class MadeTransparentImpl[T, U, E <: MadeElem.Of[U] *: EmptyTuple, C <: AnyRef | NotExists](
  val elems: E,
  unwrapFn: T => U,
  wrapFn: U => T,
  val companion: C,
) extends Made.Transparent:
  type Type = T
  type ElemType = U
  type Elems = E
  type Companion = C
  def unwrap(value: T): U = unwrapFn(value)
  def wrap(value: U): T = wrapFn(value)
