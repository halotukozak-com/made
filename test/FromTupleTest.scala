package halotukozak.made

class FromTupleTest extends munit.FunSuite:
  import FromTupleTest.*

  test("fromTuple for simple case class") {
    val m = Made.derived[Person]
    val result = m.fromTuple(("Alice", 30))
    assertEquals(result, Person("Alice", 30))
  }

  test("fromTuple for case class with no fields") {
    val m = Made.derived[Empty]
    val result = m.fromTuple(EmptyTuple)
    assertEquals(result, Empty())
  }

  test("fromTuple for generic case class") {
    val m = Made.derived[Box[String]]
    val result = m.fromTuple(Tuple("content"))
    assertEquals(result, Box("content"))
  }

  test("fromTuple for value class") {
    val m = Made.derived[Wrap]
    val result = m.fromTuple(Tuple("v"))
    assertEquals(result, Wrap("v"))
  }

  test("fromTuple for many fields") {
    val m = Made.derived[Big]
    val result = m.fromTuple((1, "two", 3.0, true, 5L))
    assertEquals(result, Big(1, "two", 3.0, true, 5L))
  }

  test("fromTuple roundtrips with Tuple.fromProductTyped") {
    val m = Made.derived[Person]
    val p = Person("Bob", 25)
    val rebuilt = m.fromTuple(Tuple.fromProductTyped(p))
    assertEquals(rebuilt, p)
  }

  test("fromTuple is type-safe at call site") {
    val m = Made.derived[Person]
    val tup: m.ElemTypes = ("Carol", 28)
    val _: Person = m.fromTuple(tup)
  }

object FromTupleTest:
  case class Person(name: String, age: Int)
  case class Empty()
  case class Box[A](a: A)
  case class Wrap(v: String) extends AnyVal
  case class Big(a: Int, b: String, c: Double, d: Boolean, e: Long)
