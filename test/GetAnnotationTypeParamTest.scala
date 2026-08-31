package halotukozak.made

import halotukozak.made.annotation.*

import scala.compiletime.testing.typeCheckErrors

/**
 * Does `getAnnotation` preserve a type parameter of the annotation class itself, as opposed to
 * just the parameter *values*?
 *
 * `class Annot[Name <: String](val name: Name)` is queried via `getAnnotation[Annot[SomeArg]]`,
 * where `SomeArg` participates in the lookup's `annot.tpe <:< TypeRepr.of[A]` subtype check
 * (see [[halotukozak.made.extensions.getAnnotationImpl]]/`findAnnotationExpr`). Two things fall out of that:
 *
 *   - When the found annotation's type is a subtype of the requested `A`, `transparent inline`
 *     narrows the result to the annotation's *actual* precise type, not to `A` as written —
 *     exactly like it already does for parameter-less annotations (see
 *     [[halotukozak.made.FieldAnnotationTest]]'s "narrows to the annotation type or NotExists" tests).
 *   - Because `Name` is invariant here, that subtype check only succeeds if `A`'s type argument
 *     matches exactly (or the argument is a wildcard). Querying with a *widened* argument acts as
 *     asking for a different, unrelated type: the annotation isn't found and `getAnnotation`
 *     narrows to `NotExists`. A covariant type parameter does not have this restriction.
 */
class GetAnnotationTypeParamTest extends munit.FunSuite:

  test("getAnnotation preserves the annotation's literal type argument") {
    val mirror = Made.derived[Annotated]
    val a: Annot["foo"] = mirror.getAnnotation[Annot["foo"]]
    assertEquals(a.name: "foo", "foo")
  }

  test("getAnnotation narrows to the precise type argument, not a wider ascription") {
    val errors = typeCheckErrors("""
      val mirror = Made.derived[Annotated]
      val a: Annot[String] = mirror.getAnnotation[Annot["foo"]]
    """)
    assert(
      errors.nonEmpty,
      "expected a type mismatch: getAnnotation narrows to Annot[\"foo\"], not the wider Annot[String]",
    )
  }

  test("invariant type parameter: querying with a widened type argument does not find the annotation") {
    val mirror = Made.derived[Annotated]
    // Annot["foo"] is not a subtype of the invariant Annot[String], so the lookup's
    // `annot.tpe <:< TypeRepr.of[A]` check fails and the annotation is reported as absent.
    val a: NotExists.type = mirror.getAnnotation[Annot[String]]
    assertEquals(a, NotExists)
  }

  test("invariant type parameter: querying with a mismatched literal does not find the annotation") {
    val mirror = Made.derived[Annotated]
    val a: NotExists.type = mirror.getAnnotation[Annot["bar"]]
    assertEquals(a, NotExists)
  }

  test("invariant type parameter: a wildcard type argument finds the annotation and keeps its precise type") {
    val mirror = Made.derived[Annotated]
    val a = mirror.getAnnotation[Annot[?]]
    assert(a != NotExists)
    assertEquals(a.name, "foo")
    assertEquals(a.name: "foo", "foo") // still narrowed to the literal, despite the `?` in the query
  }

  test("covariant type parameter: a widened type argument finds the annotation and keeps its precise type") {
    val mirror = Made.derived[CovAnnotated]
    val a: CovAnnot["bar"] = mirror.getAnnotation[CovAnnot[String]]
    assertEquals(a.name: "bar", "bar")
  }

  test("hasAnnotation follows the same invariance rules as getAnnotation") {
    val mirror = Made.derived[Annotated]
    assert(mirror.hasAnnotation[Annot["foo"]])
    assert(!mirror.hasAnnotation[Annot["bar"]])
    assert(!mirror.hasAnnotation[Annot[String]])
    assert(mirror.hasAnnotation[Annot[?]])

    val covMirror = Made.derived[CovAnnotated]
    assert(covMirror.hasAnnotation[CovAnnot[String]])
  }

// --- Fixtures ---

class Annot[Name <: String](val name: Name) extends MetaAnnotation

@Annot["foo"]("foo")
case class Annotated(x: Int)

class CovAnnot[+Name <: String](val name: Name) extends MetaAnnotation

@CovAnnot["bar"]("bar")
case class CovAnnotated(x: Int)
