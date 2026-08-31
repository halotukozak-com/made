package halotukozak.made

import halotukozak.made.annotation.*

import scala.compiletime.testing.typeCheckErrors

class FieldAnnotationTest extends munit.FunSuite:

  // --- Field-level annotation queries ---

  test("hasAnnotation on annotated field") {
    val mirror = Made.derived[AnnotatedFields]
    val x *: y *: z *: EmptyTuple = mirror.elems
    assert(x.hasAnnotation[Marker])
    assert(!y.hasAnnotation[Marker])
    assert(z.hasAnnotation[Marker])
  }

  test("getAnnotation on annotated field returns instance") {
    val mirror = Made.derived[AnnotatedFields]
    val x *: _ *: _ *: EmptyTuple = mirror.elems
    assert(x.getAnnotation[Marker] != NotExists)
  }

  test("getAnnotation returns NotExists on unannotated field") {
    val mirror = Made.derived[AnnotatedFields]
    val _ *: y *: _ *: EmptyTuple = mirror.elems
    assert(y.getAnnotation[Marker] == NotExists)
  }

  test("parametrized annotation on field") {
    val mirror = Made.derived[FieldWithParamAnnotation]
    val x *: y *: EmptyTuple = mirror.elems
    assertEquals(x.getAnnotation[Tag].value, "primary")
    assertEquals(y.getAnnotation[Tag].value, "secondary")
  }

  test("multiple annotations on a single field") {
    val mirror = Made.derived[MultiAnnotatedField]
    val x *: EmptyTuple = mirror.elems
    assert(x.hasAnnotation[Marker])
    assert(x.hasAnnotation[Tag])
    assertEquals(x.getAnnotation[Tag].value, "tagged")
  }

  test("field annotation does not bleed to type-level metadata") {
    val mirror = Made.derived[AnnotatedFields]
    assert(!mirror.hasAnnotation[Marker])
  }

  test("type-level annotation with field-level annotation are independent") {
    val mirror = Made.derived[BothLevelsAnnotated]
    assert(mirror.hasAnnotation[Tag])
    assertEquals(mirror.getAnnotation[Tag].value, "type-level")
    val x *: EmptyTuple = mirror.elems
    assert(x.hasAnnotation[Tag])
    assertEquals(x.getAnnotation[Tag].value, "field-level")
  }

  // --- Annotations on sum subtypes ---

  test("annotations on enum cases") {
    val mirror = Made.derived[AnnotatedEnum]
    val a *: b *: EmptyTuple = mirror.elems
    assert(a.hasAnnotation[Tag])
    assertEquals(a.getAnnotation[Tag].value, "first")
    assert(!b.hasAnnotation[Tag])
  }

  test("annotations on sealed trait subtypes") {
    val mirror = Made.derived[AnnotatedADT]
    val x *: y *: EmptyTuple = mirror.elems
    assert(x.hasAnnotation[Marker])
    assert(!y.hasAnnotation[Marker])
  }

  // --- @name combined with other annotations on fields ---

  test("@name and custom annotation on same field") {
    val mirror = Made.derived[NameAndAnnotation]
    val x *: EmptyTuple = mirror.elems
    assertEquals(x.label, "renamed")
    assert(x.hasAnnotation[Marker])
  }

  // --- Annotation on @generated member ---

  test("field annotation on generated member") {
    val mirror = Made.derived[GeneratedWithFieldAnnotation]
    val gen *: EmptyTuple = mirror.generatedElems
    assert(gen.hasAnnotation[Tag])
    assertEquals(gen.getAnnotation[Tag].value, "computed")
    assert(gen.hasAnnotation[generated])
  }

  // --- Compile-time singleton narrowing ---

  test("hasAnnotation narrows to singleton true/false") {
    val mirror = Made.derived[AnnotatedFields]
    val x *: y *: _ *: EmptyTuple = mirror.elems
    val b1: true = x.hasAnnotation[Marker]
    val b2: false = y.hasAnnotation[Marker]
    assert(b1)
    assert(!b2)
  }

  test("getAnnotation narrows to the annotation type or NotExists") {
    val mirror = Made.derived[AnnotatedFields]
    val x *: y *: _ *: EmptyTuple = mirror.elems
    val s: Marker = x.getAnnotation[Marker]
    val n: NotExists.type = y.getAnnotation[Marker]
    assert(s.isInstanceOf[Marker])
    assertEquals(n, NotExists)
  }

  // --- Negative-compile checks: narrowing is exact, not just "wide enough" ---
  //
  // The `val s: Marker = ...` / `val n: NotExists.type = ...` tests above only prove the actual
  // type is a *subtype* of the ascription. These prove the other direction — that the macro
  // doesn't widen to `Marker | NotExists` (which would also satisfy a `Marker` or `NotExists.type`
  // ascription's dual, `Any`), by checking that swapping the two ascriptions fails to typecheck.

  test("getAnnotation on a present field does not typecheck as NotExists") {
    val errors = typeCheckErrors("""
      val mirror = Made.derived[AnnotatedFields]
      val x *: _ *: _ *: EmptyTuple = mirror.elems
      val n: NotExists.type = x.getAnnotation[Marker]
    """)
    assert(errors.nonEmpty, "expected a type mismatch: a present annotation narrows to Marker, not NotExists")
  }

  test("getAnnotation on an absent field does not typecheck as the annotation type") {
    val errors = typeCheckErrors("""
      val mirror = Made.derived[AnnotatedFields]
      val _ *: y *: _ *: EmptyTuple = mirror.elems
      val m: Marker = y.getAnnotation[Marker]
    """)
    assert(errors.nonEmpty, "expected a type mismatch: an absent annotation narrows to NotExists, not Marker")
  }

  test("getAnnotations tuple elements are narrowed individually, not widened to a common A | NotExists") {
    val errors = typeCheckErrors("""
      val mirror = Made.derived[AnnotatedFields]
      val opts: (Marker, Marker, Marker) = mirror.elems.getAnnotations[Marker]
    """)
    assert(errors.nonEmpty, "expected a type mismatch: the middle element (unannotated) narrows to NotExists")
  }

  test("getAnnotation result pattern-matches against NotExists, no .get/.map needed") {
    val mirror = Made.derived[AnnotatedFields]
    val x *: y *: _ *: EmptyTuple = mirror.elems

    def describe(annot: Marker | NotExists): String = annot match
      case NotExists => "unmarked"
      case m: Marker => s"marked: ${m.getClass.getSimpleName}"

    assertEquals(describe(x.getAnnotation[Marker]), "marked: Marker")
    assertEquals(describe(y.getAnnotation[Marker]), "unmarked")
  }

  test("inline if on hasAnnotation specialises at compile time") {
    val mirror = Made.derived[AnnotatedFields]
    val x *: y *: _ *: EmptyTuple = mirror.elems
    inline def tag(inline has: Boolean): String =
      inline if has then "yes" else "no"

    assertEquals(tag(x.hasAnnotation[Marker]), "yes")
    assertEquals(tag(y.hasAnnotation[Marker]), "no")
  }

  // --- Plural tuple-of-elements queries ---

  test("hasAnnotations returns tuple of singleton booleans") {
    val mirror = Made.derived[AnnotatedFields]
    val flags: (true, false, true) = mirror.elems.hasAnnotations[Marker]
    assertEquals(flags, (true, false, true))
  }

  test("getAnnotations returns tuple narrowed per-element to the annotation type or NotExists") {
    val mirror = Made.derived[AnnotatedFields]
    val opts: (Marker, NotExists.type, Marker) = mirror.elems.getAnnotations[Marker]
    assert(opts._1.isInstanceOf[Marker])
    assertEquals(opts._2, NotExists)
    assert(opts._3.isInstanceOf[Marker])
  }

  test("hasAnnotations / getAnnotations narrow per-element") {
    val mirror = Made.derived[FieldWithParamAnnotation]
    val flags: (true, true) = mirror.elems.hasAnnotations[Tag]
    val opts: (Tag, Tag) = mirror.elems.getAnnotations[Tag]
    assertEquals(flags, (true, true))
    assertEquals(opts._1.value, "primary")
    assertEquals(opts._2.value, "secondary")
  }

  test("hasAnnotations on EmptyTuple") {
    val flags: EmptyTuple = EmptyTuple.hasAnnotations[Marker]
    assertEquals(flags, EmptyTuple)
  }

// --- Fixtures ---

class Marker extends MetaAnnotation
case class Tag(value: String) extends MetaAnnotation

case class AnnotatedFields(@Marker x: Int, y: String, @Marker z: Boolean)
case class FieldWithParamAnnotation(@Tag("primary") x: String, @Tag("secondary") y: String)
case class MultiAnnotatedField(@Marker @Tag("tagged") x: Int)

@Tag("type-level")
case class BothLevelsAnnotated(@Tag("field-level") x: Int)

enum AnnotatedEnum:
  @Tag("first") case A
  case B

sealed trait AnnotatedADT
object AnnotatedADT:
  @Marker case class X(v: Int) extends AnnotatedADT
  case class Y(v: String) extends AnnotatedADT

case class NameAndAnnotation(@name("renamed") @Marker field: Int)

case class GeneratedWithFieldAnnotation(x: Int):
  @generated @Tag("computed") def computed: String = x.toString
