package halotukozak.made

class SumOrdinalTest extends munit.FunSuite:
  import SumOrdinalTest.*

  test("Sum.ordinal returns subtype index for enum") {
    val m = Made.derived[Color]
    assertEquals(m.ordinal(Color.Red), 0)
    assertEquals(m.ordinal(Color.Green), 1)
    assertEquals(m.ordinal(Color.Blue), 2)
  }

  test("Sum.ordinal for sealed trait with mixed cases") {
    val m = Made.derived[Event]
    assertEquals(m.ordinal(Event.Created("x")), 0)
    assertEquals(m.ordinal(Event.Updated(1)), 1)
    assertEquals(m.ordinal(Event.Deleted), 2)
  }

  test("Sum.ordinal agrees with Mirror.SumOf") {
    val m = Made.derived[Color]
    val mirror = summon[scala.deriving.Mirror.SumOf[Color]]
    for c <- List(Color.Red, Color.Green, Color.Blue) do assertEquals(m.ordinal(c), mirror.ordinal(c))
  }

  test("Sum.ordinal for recursive ADT") {
    val m = Made.derived[Tree]
    assertEquals(m.ordinal(Tree.Leaf), 0)
    assertEquals(m.ordinal(Tree.Node(Tree.Leaf, Tree.Leaf)), 1)
  }

object SumOrdinalTest:
  enum Color:
    case Red, Green, Blue

  sealed trait Event
  object Event:
    case class Created(name: String) extends Event
    case class Updated(version: Int) extends Event
    case object Deleted extends Event

  enum Tree:
    case Leaf
    case Node(left: Tree, right: Tree)
