package halotukozak.made

import halotukozak.made.annotation.*

import scala.compiletime.testing.typeCheckErrors

class DefaultTypeTest extends munit.FunSuite:
  import DefaultTypeTest.*

  test("Default is refined to the field type or NotExists") {
    val (x, y, z) = Made.derived[WithDefaults].elems

    summon[x.Default =:= NotExists]
    summon[y.Default =:= String]
    summon[z.Default =:= Boolean]
  }

  test("@whenAbsent and @optionalParam refine Default to the field type") {
    val (a, b, c) = Made.derived[MixedWhenAbsent].elems
    summon[a.Default =:= NotExists]
    summon[b.Default =:= Int]
    summon[c.Default =:= String]

    val (x, y) = Made.derived[OptionalParams].elems
    summon[x.Default =:= Option[Int]]
    summon[y.Default =:= (String | Null)]
  }

  test("generated members have Default = NotExists") {
    val g *: EmptyTuple = Made.derived[WithDefaultGenerated].generatedElems
    summon[g.Default =:= NotExists]
  }

  test("named tuple fields have Default = NotExists") {
    val (a, b) = Made.derived[(a: Int, b: String)].elems
    summon[a.Default =:= NotExists]
    summon[b.Default =:= NotExists]
  }

  test("inline match on default reduces statically") {
    val (x, y, _) = Made.derived[WithDefaults].elems
    assertEquals(hasDefault(x), false)
    assertEquals(hasDefault(y), true)
  }

  test("inline code collects defaults when all fields have one") {
    assertEquals(collectDefaults(Made.derived[AllDefaults].elems), (1, "test"))
  }

  test("inline code rejects a field without a default at compile time") {
    val errors = typeCheckErrors("collectDefaults(Made.derived[WithDefaults].elems)")
    assertEquals(errors.map(_.message), List("Cannot derive defaults: x has no default value."))
  }

object DefaultTypeTest:
  case class OptionalParams(@optionalParam x: Option[Int], @optionalParam y: String | Null)

  transparent inline def hasDefault(field: MadeFieldElem): Boolean = inline field.default match
    case _: NotExists => false
    case _ => true

  transparent inline def collectDefaults[Es <: Tuple](elems: Es): Tuple = inline compiletime.erasedValue[Es] match
    case _: (head *: tail) =>
      val nonEmpty = elems.asInstanceOf[head *: tail]
      inline nonEmpty.head.asInstanceOf[head & MadeFieldElem].default match
        case _: NotExists =>
          compiletime.error(
            "Cannot derive defaults: " + compiletime.constValue[MadeElem.ExtractLabel[head]] + " has no default value.",
          )
        case default => default *: collectDefaults(nonEmpty.tail)
    case _: EmptyTuple => EmptyTuple
