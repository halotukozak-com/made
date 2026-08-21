package halotukozak.made

import halotukozak.made.annotation.*
import scala.compiletime.testing.typeCheckErrors

class RuntimeAccessTest extends munit.FunSuite:

  // --- Type erasure: what works and what doesn't ---

  test("Seq[MadeElem].map(_.label) does not compile - label requires concrete type") {
    val errors = typeCheckErrors("""
      val m = Made.derived[RAProduct]
      val elems: List[MadeElem] = m.elems.toList.asInstanceOf[List[MadeElem]]
      elems.map(_.label)
    """)
    assert(errors.nonEmpty, "Expected compile error for erased label access")
  }

  test("Seq[MadeElem].map(_.hasAnnotation[...]) does not compile - abstract Metadata") {
    val errors = typeCheckErrors("""
      val m = Made.derived[RAAnnotated]
      val elems: List[MadeElem] = m.elems.toList.asInstanceOf[List[MadeElem]]
      elems.map(_.hasAnnotation[RAMarker])
    """)
    assert(errors.nonEmpty, "Expected compile error: containsOnly Meta evidence missing for abstract Metadata")
  }

  test("Seq[MadeElem].map(_.getAnnotation[...]) does not compile - abstract Metadata") {
    val errors = typeCheckErrors("""
      val m = Made.derived[RAAnnotated]
      val elems: List[MadeElem] = m.elems.toList.asInstanceOf[List[MadeElem]]
      elems.map(_.getAnnotation[RAMarker])
    """)
    assert(errors.nonEmpty, "Expected compile error: containsOnly Meta evidence missing for abstract Metadata")
  }

  // --- elems.toList / toArray ---

  test("elems.toList for product") {
    val m = Made.derived[RAProduct]
    val elems = m.elems.toList.asInstanceOf[List[MadeFieldElem]]
    assertEquals(elems.size, 3)
  }

  test("elems.toList for enum") {
    val m = Made.derived[RAEnum]
    val elems = m.elems.toList.asInstanceOf[List[MadeElem]]
    assertEquals(elems.size, 3)
  }

  test("elems.toList for singleton is empty") {
    val m = Made.derived[RAObject.type]
    assert(m.elems.toList.isEmpty)
  }

  test("elems.toArray") {
    val m = Made.derived[RAProduct]
    assertEquals(m.elems.toArray.length, 3)
  }

  test("generatedElems.toList") {
    val m = Made.derived[RAWithGenerated]
    val genElems = m.generatedElems.toList.asInstanceOf[List[GeneratedMadeElem]]
    assertEquals(genElems.size, 2)
  }

  // --- Runtime .default works on erased MadeFieldElem ---

  test("Seq[MadeFieldElem].map(_.default) works at runtime") {
    val m = Made.derived[RAWithDefaults]
    val elems = m.elems.toList.asInstanceOf[List[MadeFieldElem]]
    val defaults = elems.map(_.default)
    assertEquals(defaults, List(NotExists, "hello", true))
  }

  test("Seq[GeneratedMadeElem].map(_.default) all None") {
    val m = Made.derived[RAWithGenerated]
    val genElems = m.generatedElems.toList.asInstanceOf[List[GeneratedMadeElem]]
    assertEquals(genElems.map(_.default), List(NotExists, NotExists))
  }

  // --- Runtime .value works on erased MadeSubSingletonElem ---

  test("collect singleton values from erased Seq") {
    val m: Made.Sum {
      type Type = RAEnum
      type Label = "RAEnum"
      type Metadata = EmptyTuple
      type Elems = MadeSubSingletonElem {
        type Type = RAEnum.A.type
        type Label = "A"
        type Metadata = EmptyTuple
      } *: MadeSubSingletonElem {
        type Type = RAEnum.B.type
        type Label = "B"
        type Metadata = EmptyTuple
      } *: MadeSubElem {
        type Type = RAEnum.C
        type Label = "C"
        type Metadata = EmptyTuple
      } *: EmptyTuple
    } = Made.derived[RAEnum]

    val singletons = m.elems.toList.collect { case s: MadeSubSingletonElem => s.value }
    assertEquals(singletons.size, 2)
    assert(singletons.contains(RAEnum.A))
    assert(singletons.contains(RAEnum.B))
  }

  // --- elemLabels.toList is the correct alternative to erased _.label ---

  test("elemLabels.toList provides labels as List[String]") {
    val m = Made.derived[RAProduct]
    val labels = m.elemLabels.toList.asInstanceOf[List[String]]
    assertEquals(labels, List("x", "y", "z"))
  }

  test("elemLabels.toList with @name overrides") {
    val m = Made.derived[RANamed]
    val labels = m.elemLabels.toList.asInstanceOf[List[String]]
    assertEquals(labels, List("renamed", "b"))
  }

  // --- Zip labels with defaults ---

  test("zip elemLabels with defaults at runtime") {
    val m = Made.derived[RAWithDefaults]
    val labels = m.elemLabels.toList.asInstanceOf[List[String]]
    val elems = m.elems.toList.asInstanceOf[List[MadeFieldElem]]
    val zipped = labels.zip(elems.map(_.default))
    assertEquals(zipped, List(("x", NotExists), ("y", "hello"), ("z", true)))
  }

  // --- Seq[Made] ---

  test("store multiple Made mirrors in Seq[Made]") {
    val mirrors: Seq[Made] = Seq(
      Made.derived[RAProduct],
      Made.derived[RAEnum],
      Made.derived[RAObject.type],
    )
    assertEquals(mirrors.size, 3)
  }

  test("Seq[Made] - elems.toList sizes") {
    val mirrors: Seq[Made] = Seq(
      Made.derived[RAProduct],
      Made.derived[RAEmpty],
      Made.derived[RAObject.type],
    )
    assertEquals(mirrors.map(_.elems.toList.size), Seq(3, 0, 0))
  }

  test("Seq[Made] - generatedElems.toList sizes") {
    val mirrors: Seq[Made] = Seq(
      Made.derived[RAProduct],
      Made.derived[RAWithGenerated],
    )
    assertEquals(mirrors.map(_.generatedElems.toList.size), Seq(0, 2))
  }

  // --- Runtime generated elem apply ---

  test("iterate generatedElems.toList and apply to instance") {
    val m = Made.derived[RAWithGenerated]
    val instance = RAWithGenerated("test")
    val genElems = m.generatedElems.toList.asInstanceOf[List[GeneratedMadeElem { type OuterType = RAWithGenerated }]]
    val results = genElems.map(_.apply(instance))
    assertEquals(results, List(4, "TEST"))
  }

  // --- Round-trip: defaults -> fromUnsafeArray ---

  test("round-trip: collect defaults and build instance") {
    val m = Made.derived[RAAllDefaults]
    val elems = m.elems.toList.asInstanceOf[List[MadeFieldElem]]
    val defaults = elems.map(_.default)
    val instance = m.fromUnsafeArray(defaults.toArray)
    assertEquals(instance, RAAllDefaults())
  }

  // --- Transparent in Seq[Made] ---

  test("transparent mirror in Seq[Made]") {
    val m = Made.derived[RATransparent]
    val mirrors: Seq[Made] = Seq(m)
    assertEquals(mirrors.head.elems.toList.size, 1)
  }

// --- Fixtures ---

case class RAProduct(x: Int, y: String, z: Boolean)
case class RAEmpty()
case object RAObject
case class RANamed(@name("renamed") a: Int, b: String)

class RAMarker extends MetaAnnotation
case class RAAnnotated(@RAMarker x: Int, y: String)

case class RAWithDefaults(x: Int, y: String = "hello", z: Boolean = true)
case class RAAllDefaults(a: Int = 1, b: String = "default")

case class RAWithGenerated(s: String):
  @generated def len: Int = s.length
  @generated def upper: String = s.toUpperCase

enum RAEnum:
  case A, B
  case C(value: Int)

@transparent
case class RATransparent(value: Int)
