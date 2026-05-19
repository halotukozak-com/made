package made

import made.annotation.*

class ElemIndexTest extends munit.FunSuite:
  import ElemIndexTest.*

  test("MadeFieldElem.index reports zero-based constructor position") {
    val m = Made.derived[Person]
    val name *: age *: email *: EmptyTuple = m.elems
    assertEquals(name.index, 0)
    assertEquals(age.index, 1)
    assertEquals(email.index, 2)
  }

  test("MadeFieldElem.Index is a singleton literal type") {
    val m = Made.derived[Person]
    val name *: age *: _ = m.elems
    summon[name.Index =:= 0]
    summon[age.Index =:= 1]
  }

  test("index for value class is 0") {
    val m = Made.derived[Wrap]
    val v *: EmptyTuple = m.elems
    assertEquals(v.index, 0)
    summon[v.Index =:= 0]
  }

  test("index for @transparent case class is 0") {
    val m = Made.derived[Id]
    val inner *: EmptyTuple = m.elems
    assertEquals(inner.index, 0)
  }

  test("GeneratedMadeElem.index reflects position in GeneratedElems") {
    val m = Made.derived[WithGen]
    val a *: b *: EmptyTuple = m.generatedElems
    assertEquals(a.index, 0)
    assertEquals(b.index, 1)
  }

object ElemIndexTest:
  case class Person(name: String, age: Int, email: String)
  case class Wrap(v: String) extends AnyVal
  @transparent case class Id(value: Long)

  case class WithGen(x: Int):
    @generated def doubled: Int = x * 2
    @generated def stringified: String = x.toString
