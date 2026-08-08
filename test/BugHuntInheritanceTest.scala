package made

import made.annotation.*

import scala.annotation.StaticAnnotation

class BugHuntInheritanceTest extends munit.FunSuite:
  import BugHuntInheritanceTest.*

  test("@mongoId aggregate annotation declared on a parent trait propagates to child field") {
    val m = Made.derived[ChildWithMongo]
    val id *: EmptyTuple = m.elems
    assertEquals(id.label, "_id")
  }

  test("@whenAbsent on a parent trait member is honoured on the child case class field") {
    val m = Made.derived[ChildWithDefault]
    val n *: EmptyTuple = m.elems
    assertEquals(n.default, Some(42))
  }

  test("MetaAnnotation declared on a parent trait shows up in child Metadata") {
    val m = Made.derived[ChildWithJson]
    val x *: EmptyTuple = m.elems
    assert(x.hasAnnotation[JsonName])
    assertEquals(x.getAnnotation[JsonName].value, "inherited")
  }

  test("nested aggregate — outer aggregate yields inner aggregate's inner annotations") {
    val m = Made.derived[ChildWithNested]
    val id *: EmptyTuple = m.elems
    assertEquals(id.label, "_id")
  }

  test("aggregate annotation on the case class itself overrides Label") {
    val m = Made.derived[NamedByAggregate]
    assertEquals(m.label, "custom-aggregate")
  }

  test("multiple aggregate annotations on same field merge their inner annotations") {
    val m = Made.derived[MultiAggregate]
    val f *: EmptyTuple = m.elems
    assertEquals(f.label, "_id")
    assert(f.hasAnnotation[JsonName])
  }

  test("child case-class field annotation does not erase parent trait annotation") {
    val m = Made.derived[ChildOverridesAnnotation]
    val x *: EmptyTuple = m.elems
    val js = x.getAnnotation[JsonName].value
    assert(js == "child" || js == "parent", s"expected child or parent JsonName, got $js")
  }

  test("aggregate annotation on enum case sets the case label") {
    val m = Made.derived[Anim]
    val cat *: dog *: EmptyTuple = m.elems
    assertEquals(cat.label, "feline")
    assertEquals(dog.label, "Dog")
  }

object BugHuntInheritanceTest:
  class JsonName(val value: String) extends MetaAnnotation

  class mongoId extends AnnotationAggregate:
    @name("_id")
    final def aggregated: List[StaticAnnotation] = reifyAggregated

  trait HasMongo:
    @mongoId
    def id: String

  case class ChildWithMongo(id: String) extends HasMongo

  trait HasDefault:
    @whenAbsent(42)
    def n: Int

  case class ChildWithDefault(n: Int) extends HasDefault

  trait HasJson:
    @JsonName("inherited")
    def x: Int

  case class ChildWithJson(x: Int) extends HasJson

  // Nested aggregate: outer wraps an inner aggregate
  class innerNamer extends AnnotationAggregate:
    @name("_id")
    final def aggregated: List[StaticAnnotation] = reifyAggregated

  class outerNamer extends AnnotationAggregate:
    @innerNamer
    final def aggregated: List[StaticAnnotation] = reifyAggregated

  case class ChildWithNested(@outerNamer id: String)

  // Aggregate on the case class itself, providing a @name
  class customNameAggregate extends AnnotationAggregate:
    @name("custom-aggregate")
    final def aggregated: List[StaticAnnotation] = reifyAggregated

  @customNameAggregate
  case class NamedByAggregate(x: Int)

  // Two aggregates on one field, contributing different annotations
  class jsonAggregate extends AnnotationAggregate:
    @JsonName("via-aggregate")
    final def aggregated: List[StaticAnnotation] = reifyAggregated

  case class MultiAggregate(@innerNamer @jsonAggregate field: String)

  trait HasJsonChild:
    @JsonName("parent")
    def x: Int

  case class ChildOverridesAnnotation(@JsonName("child") x: Int) extends HasJsonChild

  enum Anim:
    @innerNamer @name("feline") case Cat
    case Dog
