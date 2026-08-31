package halotukozak.made

import halotukozak.made.annotation.*
import halotukozak.commons.containsOnly

class PathDependentEvidenceTest extends munit.FunSuite:
  import PathDependentEvidenceTest.*

  // --- Made: path-dependent givens ---

  test("Made: Elems containsOnly MadeElem summons without import") {
    val m = Made.derived[PProduct]
    summon[m.Elems containsOnly MadeElem]
  }

  test("Made: Metadata containsOnly Meta summons without import") {
    val m = Made.derived[PAnnotated]
    summon[m.Metadata containsOnly Meta]
  }

  test("Made: GeneratedElems containsOnly GeneratedMadeElem summons without import") {
    val m = Made.derived[PWithGenerated]
    summon[m.GeneratedElems containsOnly GeneratedMadeElem]
  }

  test("Made.Sum: Elems containsOnly MadeSubElem summons without import") {
    val m = Made.derived[PEnum]
    summon[m.Elems containsOnly MadeSubElem]
  }

  test("Made.Transparent: Elems containsOnly MadeFieldElem summons without import") {
    val m = Made.derived[PWrap]
    summon[m.Elems containsOnly MadeFieldElem]
  }

  test("Made: GeneratedElems containsOnly MadeElem via contravariance") {
    val m = Made.derived[PWithGenerated]
    summon[m.GeneratedElems containsOnly MadeElem]
  }

  test("Made: generatedElems.hasAnnotations works") {
    val m = Made.derived[PWithGenerated]
    val flags: (true, true) = m.generatedElems.hasAnnotations[generated]
    assertEquals(flags, (true, true))
  }

  test("Made: generatedElems.getAnnotations works") {
    val m = Made.derived[PWithGenerated]
    val opts: (generated, generated) = m.generatedElems.getAnnotations[generated]
    assert(opts._1.isInstanceOf[generated])
    assert(opts._2.isInstanceOf[generated])
  }

  test("MadeElem: Metadata containsOnly Meta summons without import") {
    val m = Made.derived[PAnnotated]
    val x *: _ = m.elems
    summon[x.Metadata containsOnly Meta]
  }

  // --- Done: path-dependent givens ---

  test("Done: Metadata containsOnly Meta summons without import") {
    val d = Done.derived[PSvc]
    summon[d.Metadata containsOnly Meta]
  }

  test("Done: Operations containsOnly DoneOperation summons without import") {
    val d = Done.derived[PSvc]
    summon[d.Operations containsOnly DoneOperation]
  }

  test("Done: operations.hasAnnotations works via structural Metadata") {
    val d = Done.derived[PSvc]
    val flags: (true, false) = d.operations.hasAnnotations[PMarker]
    assertEquals(flags, (true, false))
  }

  test("Done: operations.getAnnotations works via structural Metadata") {
    val d = Done.derived[PSvc]
    val opts: (PMarker, NotExists.type) = d.operations.getAnnotations[PMarker]
    assert(opts._1.isInstanceOf[PMarker])
    assertEquals(opts._2, NotExists)
  }

  test("hasAnnotations works on arbitrary tuple of refined types with Metadata member") {
    // Proves the extension is purely structural — no MadeElem/InputElem dependency.
    // Uses the same refinement shape that real mirrors produce (e.g. `MadeFieldElem { type Metadata = ... }`).
    trait Holder:
      type Metadata <: Tuple

    val a: Holder { type Metadata = (Meta @PMarker) *: EmptyTuple } =
      new Holder:
        type Metadata = (Meta @PMarker) *: EmptyTuple
    val b: Holder { type Metadata = EmptyTuple } =
      new Holder:
        type Metadata = EmptyTuple
    val c: Holder { type Metadata = (Meta @PMarker) *: (Meta @PMarker) *: EmptyTuple } =
      new Holder:
        type Metadata = (Meta @PMarker) *: (Meta @PMarker) *: EmptyTuple

    val tup = (a, b, c)
    val flags: (true, false, true) = tup.hasAnnotations[PMarker]
    val opts: (PMarker, NotExists.type, PMarker) = tup.getAnnotations[PMarker]
    assertEquals(flags, (true, false, true))
    assert(opts._1.isInstanceOf[PMarker])
    assertEquals(opts._2, NotExists)
    assert(opts._3.isInstanceOf[PMarker])
  }

  test("hasAnnotations does not compile on tuple element lacking Metadata member") {
    val errors = scala.compiletime.testing.typeCheckErrors("""
      val tup: (String, Int) = ("x", 1)
      tup.hasAnnotations[halotukozak.made.PathDependentEvidenceTest.PMarker]
    """)
    assert(errors.nonEmpty, "Expected compile error: no Metadata member on String/Int")
  }

  test("DoneOperation: Metadata containsOnly Meta summons without import") {
    val d = Done.derived[PSvc]
    val op *: _ = d.operations
    summon[op.Metadata containsOnly Meta]
  }

  test("DoneOperation: InputElems containsOnly InputElem summons without import") {
    val d = Done.derived[PSvc]
    val op *: _ = d.operations
    summon[op.InputElems containsOnly InputElem]
  }

  test("InputElem: Metadata containsOnly Meta summons without import") {
    val d = Done.derived[PSvc]
    val _ *: op2 *: EmptyTuple = d.operations
    val ie *: _ = op2.inputElems
    summon[ie.Metadata containsOnly Meta]
  }

object PathDependentEvidenceTest:
  class PMarker extends MetaAnnotation

  case class PProduct(a: Int, b: String)

  enum PEnum:
    case Cs(x: Int)
    case Obj

  @transparent case class PWrap(value: Int)

  @PMarker
  case class PAnnotated(@PMarker x: Int, y: String)

  case class PWithGenerated(s: String):
    @generated def len: Int = s.length
    @generated def upper: String = s.toUpperCase

  @PMarker
  trait PSvc:
    @PMarker def op1: Int
    def op2(x: String): Boolean

  trait PInputSvc:
    def compute(@PMarker tagged: String, untagged: Int): Boolean
