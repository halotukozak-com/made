package halotukozak.made

import halotukozak.made.annotation.*

import scala.compiletime.testing.typeCheckErrors

/**
 * Given `class CovAnnot[+Name <: String](val name: Name)`, once `getAnnotation` has narrowed to
 * the precise `CovAnnot["bar"]` (see [[halotukozak.made.GetAnnotationTypeParamTest]]), can `Name` itself be
 * pulled back out as a compile-time-checked singleton type — not just a correct runtime value?
 *
 * Two ways to ask "what's the `Name` of this `CovAnnot[_]`?" behave differently:
 *
 *   - A term-level `inline ... match { case _: CovAnnot[name] => constValue[name] }` reconciles
 *     fine against an expected type you already supply (`val x: "bar" = ...` typechecks), because
 *     `transparent inline` elaborates the body using that expected type as a hint. But left to
 *     infer its own type with no hint (`val x = ...`), `name` is only an existential bound by its
 *     declared upper bound (`<: String`) once outside the match case, so the result widens to
 *     `String`. That means this form can *check* a literal you already guessed, but can't *derive*
 *     an unknown one — there is no case in which it fails to compile due to the "wrong" name.
 *   - A match *type* (`type NameOf[A] = A match { case CovAnnot[n] => n }`) reduces at the type
 *     level and really does substitute `n` with `"bar"` in the result, with no hint required.
 *     `NameOf[ann.type]` genuinely *is* `"bar"` — a true compile-time extraction.
 */
class GetAnnotationCompileTimeExtractionTest extends munit.FunSuite:

  type NameOf[A] = A match
    case CovAnnot[n] => n

  test("term-level inline match reconciles with a supplied expected type") {
    val mirror = Made.derived[CovAnnotated]

    transparent inline def extractedName =
      inline mirror.getAnnotation[CovAnnot[String]] match
        case _: CovAnnot[name] => compiletime.constValue[name]

    val direct: "bar" = extractedName
    assertEquals(direct, "bar")
  }

  test("term-level inline match has no narrower natural type without a hint") {
    val errors = typeCheckErrors("""
      val mirror = Made.derived[CovAnnotated]
      transparent inline def extractedName =
        inline mirror.getAnnotation[CovAnnot[String]] match
          case _: CovAnnot[name] => compiletime.constValue[name]
      val n = extractedName // no expected type: infers extractedName's own natural type
      val x: "bar" = n // ...which turns out not to be "bar"
    """)
    assert(
      errors.nonEmpty,
      "expected a type mismatch: without a hint, extractedName's natural type is String, not \"bar\" " +
        "-- name's binding doesn't survive past the match case on its own",
    )
  }

  test("match type: NameOf[ann.type] recovers the singleton at compile time, no hint needed") {
    val mirror = Made.derived[CovAnnotated]
    val ann = mirror.getAnnotation[CovAnnot[String]]

    val n: NameOf[ann.type] = compiletime.constValue[NameOf[ann.type]]
    val asLiteral: "bar" = n // provably "bar", not just String
    assertEquals(asLiteral, "bar")

    val errors = typeCheckErrors("""
      val mirror = Made.derived[CovAnnotated]
      val ann = mirror.getAnnotation[CovAnnot[String]]
      val wrongLiteral: "nope" = compiletime.constValue[NameOf[ann.type]]
    """)
    assert(errors.nonEmpty, "expected a type mismatch: NameOf[ann.type] is provably \"bar\", not \"nope\"")
  }
