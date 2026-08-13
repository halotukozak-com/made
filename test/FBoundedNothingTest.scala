package halotukozak.made

import dotty.tools.dotc.reporting.Diagnostic
import halotukozak.made.util.SnippetCompiler
import halotukozak.made.util.SnippetCompiler.containsMessage

class FBoundedNothingTest extends munit.FunSuite:

  private def deriveSnippet(definitions: String, derivation: String): List[Diagnostic] =
    SnippetCompiler.compile:
      // language=scala 3
      s"""import halotukozak.made.*
         |$definitions
         |object FbSnippet { val _ = $derivation }
         |""".stripMargin

  private def assertWarns(definitions: String, derivation: String): Unit =
    val diags = deriveSnippet(definitions, derivation)
    assert(diags.containsMessage("F-bounded"), s"Expected F-bounded warning but got: $diags")

  private def assertSilent(definitions: String, derivation: String): Unit =
    val diags = deriveSnippet(definitions, derivation)
    assert(!diags.containsMessage("F-bounded"), s"Expected no F-bounded warning but got: $diags")

  test("covariant F-bounded case class deriving at T = Nothing should warn") {
    assertWarns(
      """trait FBound[+T]
        |case class P[+T <: FBound[T]](v: T)
        |""".stripMargin,
      "Made.derived[P[Nothing]]",
    )
  }

  test("invariant F-bounded case class deriving at T = Nothing should warn") {
    assertWarns(
      """trait FBound[T]
        |case class P[T <: FBound[T]](v: T)
        |""".stripMargin,
      "Made.derived[P[Nothing]]",
    )
  }

  test("multi-param F-bounded class deriving at Nothing on F-bounded param should warn") {
    assertWarns(
      """trait FBound[+T]
        |case class P[A, +T <: FBound[T]](a: A, v: T)
        |""".stripMargin,
      "Made.derived[P[String, Nothing]]",
    )
  }

  test("covariant F-bounded case class deriving at concrete T should not warn") {
    assertSilent(
      """trait FBound[+T]
        |case class Concrete(i: Int) extends FBound[Concrete]
        |case class P[+T <: FBound[T]](v: T)
        |""".stripMargin,
      "Made.derived[P[Concrete]]",
    )
  }

  test("invariant F-bounded case class deriving at concrete T should not warn") {
    assertSilent(
      """trait FBound[T]
        |case class Concrete(i: Int) extends FBound[Concrete]
        |case class P[T <: FBound[T]](v: T)
        |""".stripMargin,
      "Made.derived[P[Concrete]]",
    )
  }

  test("F-bound inside intersection bound deriving at Nothing should warn") {
    assertWarns(
      """trait FBound[+T]
        |case class P[+T <: FBound[T] & Serializable](v: T)
        |""".stripMargin,
      "Made.derived[P[Nothing]]",
    )
  }

  test("non-F-bounded case class deriving at T = Nothing should not warn") {
    assertSilent(
      "case class P[+T](v: T)",
      "Made.derived[P[Nothing]]",
    )
  }
