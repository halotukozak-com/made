package halotukozak.made

import halotukozak.made.annotation.*
class MadeDefaultsTest extends munit.FunSuite:
  test("case class with defaults") {
    val m: Made.Product {
      type Type = WithDefaults
      type Label = "WithDefaults"
      type Metadata = EmptyTuple
      type Elems = MadeFieldElem {
        type Type = Int
        type Label = "x"
        type Metadata = EmptyTuple
        type OuterType = WithDefaults
      } *: MadeFieldElem {
        type Type = String
        type Label = "y"
        type Metadata = EmptyTuple
        type OuterType = WithDefaults
      } *: MadeFieldElem {
        type Type = Boolean
        type Label = "z"
        type Metadata = EmptyTuple
        type OuterType = WithDefaults
      } *: EmptyTuple
    } = Made.derived[WithDefaults]

    val (x, y, z) = m.elems

    assert(x.default == NotExists)
    assert(y.default == "hello")
    assert(z.default == true)
  }

  test("case class with all defaults") {
    val m: Made.Product {
      type Type = AllDefaults
      type Label = "AllDefaults"
      type Metadata = EmptyTuple
      type Elems = MadeFieldElem {
        type Type = Int
        type Label = "a"
        type Metadata = EmptyTuple
        type OuterType = AllDefaults
      } *: MadeFieldElem {
        type Type = String
        type Label = "b"
        type Metadata = EmptyTuple
        type OuterType = AllDefaults
      } *: EmptyTuple
    } = Made.derived[AllDefaults]

    val (a, b) = m.elems

    assert(a.default == 1)
    assert(b.default == "test")
  }

  test("case class with mixed defaults") {
    val m: Made.Product {
      type Type = MixedDefaults
      type Label = "MixedDefaults"
      type Metadata = EmptyTuple
      type Elems = MadeFieldElem {
        type Type = Int
        type Label = "required"
        type Metadata = EmptyTuple
        type OuterType = MixedDefaults
      } *: MadeFieldElem {
        type Type = String
        type Label = "optional"
        type Metadata = EmptyTuple
        type OuterType = MixedDefaults
      } *: EmptyTuple
    } = Made.derived[MixedDefaults]

    val (a, b) = m.elems

    assert(a.default == NotExists)
    assert(b.default == "default")
  }

  test("GeneratedDerElem.default returns None") {
    val m = Made.derived[WithDefaultGenerated]

    val y *: EmptyTuple = m.generatedElems
    assert(y.default == NotExists)
  }

  test("@whenAbsent provides default") {
    val m = Made.derived[WithWhenAbsent]

    val (x, y) = m.elems

    assert(x.default == NotExists)
    assert(y.default == "absent")
  }

  test("@whenAbsent takes priority over Scala default value") {
    val m = Made.derived[WhenAbsentOverridesDefault]

    val (a, b) = m.elems

    assert(a.default == 42)
    assert(b.default == "fromAnnotation")
  }

  test("mixing @whenAbsent, Scala defaults, and no defaults") {
    val m = Made.derived[MixedWhenAbsent]

    val (a, b, c) = m.elems

    assert(a.default == NotExists)
    assert(b.default == 99)
    assert(c.default == "scalaDefault")
  }

  test("recursive case class with @whenAbsent") {
    val m = Made.derived[RecWithDefault.Node]

    val (value, next) = m.elems

    assert(value.default == NotExists)
    assert(next.default == None)
  }

  test("recursive case class with Scala default") {
    val m = Made.derived[RecWithScalaDefault.Node]

    val (value, next) = m.elems

    assert(value.default == NotExists)
    assert(next.default == None)
  }

  test("@optionalParam provides default from OptionLike") {
    val m = Made.derived[WithOptionalParam]

    val (x, y, z) = m.elems

    assertEquals(x.default, None)
    assertEquals(y.default, null: String | Null)
    assertEquals(z.default, NotExists)
  }

  test("@optionalParam priority") {
    val m = Made.derived[OptionalParamPriority]

    val (a, b) = m.elems

    // @whenAbsent(Some(42)) should take priority over @optionalParam
    assertEquals(a.default, Some(42))
    // @optionalParam should take priority over Scala default None
    // Wait, let's check the code:
    // fromWhenAbsent orElse fromOptionalParam orElse fromDefaultValue
    assertEquals(b.default, None)
  }

  test("generic case class with Scala default") {
    val m = Made.derived[GenericWithDefault[Int]]

    val (a, label) = m.elems

    assert(a.default == NotExists)
    assert(label.default == "default")
  }

  test("generic case class with type-dependent default") {
    val m = Made.derived[GenericPair[String]]

    val (a, b) = m.elems

    assert(a.default == NotExists)
    assert(b.default == None)
  }

  test("generic case class with @whenAbsent takes priority") {
    val m = Made.derived[GenericWhenAbsent[Int]]

    val (a, b) = m.elems

    assert(a.default == NotExists)
    assert(b.default == "annotated")
  }

  test("@optionalParam with custom Default") {
    given Default[CustomOpt[String]] = () => CustomOpt("none")

    val m = Made.derived[WithCustomOptional]
    val x *: EmptyTuple = m.elems
    assertEquals(x.default, CustomOpt("none"))
  }

case class WithDefaults(x: Int, y: String = "hello", z: Boolean = true)
case class AllDefaults(a: Int = 1, b: String = "test")
case class MixedDefaults(required: Int, optional: String = "default")
case class WithDefaultGenerated(x: Int, y: String = "hello"):
  @generated def gen: Int = x + y.length
case class WithWhenAbsent(x: Int, @whenAbsent("absent") y: String)
case class WhenAbsentOverridesDefault(
  @whenAbsent(42) a: Int = 0,
  @whenAbsent("fromAnnotation") b: String = "fromDefault",
)
case class MixedWhenAbsent(a: Int, @whenAbsent(99) b: Int, c: String = "scalaDefault")

case class WithOptionalParam(
  @optionalParam x: Option[Int],
  @optionalParam y: String | Null,
  z: Option[String],
)

case class OptionalParamPriority(
  @whenAbsent(Some(42)) @optionalParam a: Option[Int],
  @optionalParam b: Option[Int] = Some(1),
)

case class CustomOpt[A](value: A)
case class WithCustomOptional(@optionalParam x: CustomOpt[String])

object RecWithDefault:
  case class Node(value: Int, @whenAbsent(None) next: Option[Node])
object RecWithScalaDefault:
  case class Node(value: Int, next: Option[Node] = None)

case class GenericWithDefault[T](a: T, label: String = "default")
case class GenericPair[T](a: T, b: Option[T] = None)
case class GenericWhenAbsent[T](a: T, @whenAbsent("annotated") b: String = "default")
