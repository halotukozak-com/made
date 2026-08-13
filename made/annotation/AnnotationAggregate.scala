package halotukozak.made.annotation

import scala.annotation.{publicInBinary, StaticAnnotation}
import scala.quoted.*

/**
 * Base trait for annotations that aggregate multiple other annotations. An aggregate annotation
 * works like an "annotation function" — applying it to a symbol behaves as if all the annotations
 * declared on its `aggregated` method were applied directly.
 *
 * To declare an aggregate, extend `AnnotationAggregate` and implement the `aggregated` method via
 * the [[reifyAggregated]] macro. The aggregated annotations must be declared on that
 * implementation method itself. The implementation must be `final`.
 *
 * Constructor parameters of the aggregate may be referenced as arguments to inner annotations and
 * will be substituted at the application site:
 *
 * {{{
 *   class customName(name: String) extends AnnotationAggregate {
 *     @name(name)
 *     final def aggregated: List[StaticAnnotation] = reifyAggregated
 *   }
 * }}}
 *
 * Made's annotation lookup machinery (`hasAnnotation`/`getAnnotation` on mirror elements,
 * label resolution and metadata) automatically expands aggregates so that downstream code
 * sees the underlying annotations directly.
 */
trait AnnotationAggregate extends StaticAnnotation:

  /**
   * Returns the aggregated annotations. Implement with [[reifyAggregated]] and declare the
   * aggregated annotations on this method.
   */
  def aggregated: List[StaticAnnotation]

  inline protected def reifyAggregated: List[StaticAnnotation] = ${ reifyAggregatedImpl }

// $COVERAGE-OFF$
@publicInBinary
private def reifyAggregatedImpl(using quotes: Quotes): Expr[List[StaticAnnotation]] =
  import quotes.reflect.*
  val ownerMethod = Symbol.spliceOwner.owner
  val aggregatedSym = TypeRepr.of[AnnotationAggregate].typeSymbol.declaredMethod("aggregated").head
  if !ownerMethod.allOverriddenSymbols.contains(aggregatedSym) then
    report.errorAndAbort(
      "reifyAggregated macro must only be used to implement AnnotationAggregate.aggregated method",
    )
  if !ownerMethod.flags.is(Flags.Final) || ownerMethod.flags.is(Flags.FieldAccessor) then
    report.errorAndAbort(
      "AnnotationAggregate.aggregated method implemented with reifyAggregated macro must be a final def",
    )
  val staticAnnotTpe = TypeRepr.of[StaticAnnotation]
  val annotExprs: List[Expr[StaticAnnotation]] = ownerMethod.annotations.iterator
    .filter(_.tpe <:< staticAnnotTpe)
    .map(_.asExprOf[StaticAnnotation])
    .toList
  if annotExprs.isEmpty then report.warning("no aggregated annotations found on enclosing method")
  Expr.ofList(annotExprs)
// $COVERAGE-ON$
