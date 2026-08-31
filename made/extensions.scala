package halotukozak.made

import scala.annotation.{publicInBinary, Annotation}
import scala.quoted.*
import halotukozak.commons.*

extension [M <: Tuple](self: { type Metadata = M })(using M containsOnly Meta)
  /**
   * Returns `true` if the mirror's `Metadata` tuple contains an annotation of type `A`.
   *
   * Transparent inline - resolved entirely at compile time, no runtime cost.
   * `A` must extend [[halotukozak.made.annotation.MetaAnnotation]].
   */
  transparent inline def hasAnnotation[A <: Annotation]: Boolean = ${ hasAnnotationImpl[A, M] }

  /**
   * Returns the annotation instance of type `A` if the mirror's `Metadata` tuple contains one,
   * [[NotExists]] otherwise.
   *
   * Transparent inline - resolved at compile time, so at a concrete call site the result already
   * narrows to the annotation's own type (with `.value` etc. available directly, no `.get`/`.map`)
   * or to `NotExists.type`, never a widened `A | NotExists`. Only code generic over which element
   * it inspects sees the `A | NotExists` union; it recovers `A` by matching on [[NotExists]] or via
   * the `.exists` / `.notExists` extension.
   * `A` must extend [[halotukozak.made.annotation.MetaAnnotation]].
   */
  transparent inline def getAnnotation[A <: Annotation]: A | NotExists = ${ getAnnotationImpl[A, M] }

  /**
   * Collects every annotation of type `A` in the mirror's `Metadata` tuple, in declaration order;
   * an empty list when none are present.
   *
   * Transparent inline - resolved entirely at compile time, no runtime cost. `A` must extend
   * [[halotukozak.made.annotation.MetaAnnotation]].
   */
  transparent inline def getAllAnnotations[A <: Annotation]: List[A] = ${ getAllAnnotationsImpl[A, M] }

extension [L <: String](l: { type Label = L })
  /**
   * Returns the label of the mirror.
   */
  inline def label: L = compiletime.constValue[L]

extension [Ls <: Tuple](l: { type ElemLabels = Ls })
  /**
   * Returns the labels of the mirror's elements.
   */
  inline def elemLabels: Ls = compiletime.constValueTuple[Ls]

extension (es: Tuple)(using es.type containsOnly { type Metadata <: Tuple })
  /**
   * Per-element [[hasAnnotation]] over a tuple whose entries each declare a `Metadata` type member
   * (e.g. a tuple of [[MadeElem]]s, [[GeneratedMadeElem]]s, or a singleton `Made` instance's
   * `Metadata` chain).
   */
  transparent inline def hasAnnotations[A <: Annotation]: Tuple.Map[es.type, [_] =>> Boolean] =
    ${ hasAnnotationsImpl[es.type, A] }

  /**
   * Per-element [[getAnnotation]] over a tuple whose entries each declare a `Metadata` type member.
   * Each result slot narrows independently to the annotation type or to `NotExists.type`, never to
   * a common `A | NotExists`.
   */
  transparent inline def getAnnotations[A <: Annotation]: Tuple.Map[es.type, [_] =>> A | NotExists] =
    ${ getAnnotationsImpl[es.type, A] }

// $COVERAGE-OFF$
private def findAnnotationExpr[A <: Annotation: Type, M <: Tuple: Type](using quotes: Quotes): Option[Expr[A]] =
  import quotes.reflect.*

  traverseTupleType(Type.of[M]).iterator
    .map(TypeRepr.of(using _))
    .collectFirst:
      case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[A] => annot.asExprOf[A]

@publicInBinary private def getAnnotationImpl[A <: Annotation: Type, M <: Tuple: Type](using Quotes)
  : Expr[A | NotExists] =
  findAnnotationExpr[A, M].getOrElse('{ NotExists })

@publicInBinary private def getAllAnnotationsImpl[A <: Annotation: Type, M <: Tuple: Type](using quotes: Quotes)
  : Expr[List[A]] =
  import quotes.reflect.*

  Expr.ofList:
    traverseTupleType(Type.of[M]).iterator
      .map(TypeRepr.of(using _))
      .collect:
        case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[A] => annot.asExprOf[A]
      .toList

@publicInBinary private def hasAnnotationImpl[A <: Annotation: Type, M <: Tuple: Type](using Quotes): Expr[Boolean] =
  Expr(findAnnotationExpr[A, M].isDefined)

@publicInBinary private def hasAnnotationsImpl[Es <: Tuple: Type, A <: Annotation: Type](using Quotes)
  : Expr[Tuple.Map[Es, [_] =>> Boolean]] = Expr
  .ofRefinedTuple:
    traverseTupleType(Type.of[Es]).map:
      case '[type m <: Tuple; { type Metadata = m }] => hasAnnotationImpl[A, m]
  .asInstanceOf[Expr[Tuple.Map[Es, [_] =>> Boolean]]]

@publicInBinary private def getAnnotationsImpl[Es <: Tuple: Type, A <: Annotation: Type](using Quotes)
  : Expr[Tuple.Map[Es, [_] =>> A | NotExists]] = Expr
  .ofRefinedTuple:
    traverseTupleType(Type.of[Es]).map:
      case '[type m <: Tuple; { type Metadata = m }] => getAnnotationImpl[A, m]
  .asInstanceOf[Expr[Tuple.Map[Es, [_] =>> A | NotExists]]]

// $COVERAGE-ON$
