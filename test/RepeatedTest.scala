package halotukozak.made

import halotukozak.made.annotation.*

class RepeatedTest extends munit.FunSuite:
  import RepeatedTest.*

  test("varargs field exposes @repeated in Metadata") {
    val m = Made.derived[WithVarargs]
    val name *: items *: EmptyTuple = m.elems
    assert(!name.hasAnnotation[repeated])
    assert(items.hasAnnotation[repeated])
  }

  test("varargs field type is Seq[T]") {
    val m = Made.derived[WithVarargs]
    val _ *: items *: EmptyTuple = m.elems
    summon[items.Type =:= Seq[Int]]
  }

  test("non-varargs case class has no @repeated annotation") {
    val m = Made.derived[Plain]
    val a *: b *: EmptyTuple = m.elems
    assert(!a.hasAnnotation[repeated])
    assert(!b.hasAnnotation[repeated])
  }

  test("@repeated coexists with user annotations") {
    val m = Made.derived[VarargsWithUserAnnot]
    val items *: EmptyTuple = m.elems
    assert(items.hasAnnotation[repeated])
    assert(items.hasAnnotation[Tag])
  }

  test("repeated annotation is a MetaAnnotation") {
    val r = new repeated
    assert(r.isInstanceOf[MetaAnnotation])
  }

object RepeatedTest:
  class Tag extends MetaAnnotation
  case class WithVarargs(name: String, items: Int*)
  case class Plain(a: Int, b: String)
  case class VarargsWithUserAnnot(@Tag items: String*)
