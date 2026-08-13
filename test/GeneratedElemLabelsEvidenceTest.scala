package halotukozak.made

import halotukozak.made.annotation.generated
import commons.toArrayOf

class GeneratedElemLabelsEvidenceTest extends munit.FunSuite:
  import GeneratedElemLabelsEvidenceTest.*

  test("generated-elem labels feed toArrayOf[String] without containsOnly.refl") {
    // `Tuple.Map[m.GeneratedElems, MadeElem.ExtractLabel]` spells out `MadeElem`, so the general
    // `Tuple.Map[Es, ExtractLabel] containsOnly String` given is in scope — no witness needed.
    assertEquals(genLabels[WithGen].toList, List("len", "upper"))
  }

object GeneratedElemLabelsEvidenceTest:
  inline def genLabels[T](using m: Made.Of[T]): Array[String] = inline m match
    case m: Made.ProductOf[T & scala.Product] =>
      compiletime.constValueTuple[Tuple.Map[m.GeneratedElems, MadeElem.ExtractLabel]].toArrayOf[String]

  case class WithGen(s: String):
    @generated def len: Int = s.length
    @generated def upper: String = s.toUpperCase
