package made

import made.annotation.*

import scala.annotation.StaticAnnotation

class AnnotationAggregateTest extends munit.FunSuite:

  test("aggregate expands to inner @name in labels") {
    val mirror = Made.derived[AggregateWithName]
    val x *: y *: EmptyTuple = mirror.elems
    assertEquals(x.label, "_id")
    assertEquals(y.label, "data")
  }

  test("aggregate expands to inner meta annotation in metadata") {
    val mirror = Made.derived[AggregateWithMeta]
    val x *: EmptyTuple = mirror.elems
    assert(x.hasAnnotation[InnerMeta])
  }

  test("aggregate with constructor param substitutes value") {
    val mirror = Made.derived[AggregateWithParam]
    val x *: EmptyTuple = mirror.elems
    assertEquals(x.label, "custom_name")
  }

  test("getAnnotation retrieves inner annotation instance from aggregate") {
    val mirror = Made.derived[AggregateWithMeta]
    val x *: EmptyTuple = mirror.elems
    assert(x.getAnnotation[InnerMeta] != null)
  }

class mongoId extends AnnotationAggregate:
  @name("_id")
  final def aggregated: List[StaticAnnotation] = reifyAggregated

class withMeta extends AnnotationAggregate:
  @InnerMeta
  final def aggregated: List[StaticAnnotation] = reifyAggregated

class customName(n: String) extends AnnotationAggregate:
  @name(n)
  final def aggregated: List[StaticAnnotation] = reifyAggregated

class InnerMeta extends MetaAnnotation

case class AggregateWithName(@mongoId id: String, data: String)
case class AggregateWithMeta(@withMeta x: Int)
case class AggregateWithParam(@customName("custom_name") x: Int)
