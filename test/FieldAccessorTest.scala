package halotukozak.made

import halotukozak.made.annotation.*

class FieldAccessorTest extends munit.FunSuite:
  import FieldAccessorTest.*

  test("MadeFieldElem.apply reads field value from instance") {
    val m = Made.derived[Person]
    val name *: age *: EmptyTuple = m.elems

    val p = Person("Alice", 30)
    assertEquals(name.apply(p), "Alice")
    assertEquals(age.apply(p), 30)
  }

  test("MadeFieldElem.apply has typed OuterType") {
    val m = Made.derived[Person]
    val name *: _ = m.elems
    summon[name.OuterType =:= Person]
    summon[name.Type =:= String]
  }

  test("MadeFieldElem.apply for value class") {
    val m = Made.derived[Wrapper]
    val v *: EmptyTuple = m.elems
    assertEquals(v.apply(Wrapper("hello")), "hello")
  }

  test("MadeFieldElem.apply for @transparent case class") {
    val m = Made.derived[UserId]
    val inner *: EmptyTuple = m.elems
    assertEquals(inner.apply(UserId(42L)), 42L)
  }

  test("MadeFieldElem.apply for generic case class") {
    val m = Made.derived[Box[String]]
    val a *: EmptyTuple = m.elems
    assertEquals(a.apply(Box("x")), "x")
  }

  test("MadeFieldElem.apply for recursive case class") {
    val m = Made.derived[Tree]
    val v *: l *: r *: EmptyTuple = m.elems

    val leaf = Tree(1, None, None)
    val tree = Tree(0, Some(leaf), None)
    assertEquals(v.apply(tree), 0)
    assertEquals(l.apply(tree), Some(leaf))
    assertEquals(r.apply(tree), Option.empty[Tree])
  }

  test("MadeFieldElem.apply roundtrips with fromUnsafeArray") {
    val m = Made.derived[Person]
    val name *: age *: EmptyTuple = m.elems
    val p = Person("Bob", 25)
    val rebuilt = m.fromUnsafeArray(Array(name.apply(p), age.apply(p)))
    assertEquals(rebuilt, p)
  }

object FieldAccessorTest:
  case class Person(name: String, age: Int)
  case class Wrapper(v: String) extends AnyVal
  @transparent case class UserId(value: Long)
  case class Box[A](a: A)
  case class Tree(value: Int, left: Option[Tree], right: Option[Tree])
