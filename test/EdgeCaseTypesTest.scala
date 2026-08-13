package halotukozak.made

import halotukozak.made.annotation.{transparent, MetaAnnotation}
import halotukozak.made.util.SnippetCompiler
import halotukozak.made.util.SnippetCompiler.containsMessage

class EdgeCaseTypesTest extends munit.FunSuite:
  import EdgeCaseTypesTest.*

  // --- Field types ---------------------------------------------------------

  test("derives Product with opaque-type field") {
    val m = Made.derived[WithOpaque]
    val id *: name *: EmptyTuple = m.elems
    assertEquals(id.label, "id")
    assertEquals(name.label, "name")
    val v = m.fromTuple((Ids("u-1"), "Alice"))
    assertEquals(id.apply(v), Ids("u-1"))
  }

  test("derives Product with union-type field") {
    val m = Made.derived[WithUnion]
    val v *: EmptyTuple = m.elems
    assertEquals(v.label, "v")
    val readInt: Int | String = v.apply(WithUnion(42))
    val readStr: Int | String = v.apply(WithUnion("x"))
    assertEquals(readInt, 42: Int | String)
    assertEquals(readStr, "x": Int | String)
  }

  test("derives Product with intersection-type field") {
    val m = Made.derived[WithIntersection]
    val v *: EmptyTuple = m.elems
    assertEquals(v.label, "v")
    val obj = new MarkA with MarkB
    assertEquals(v.apply(WithIntersection(obj)), obj)
  }

  test("derives transparent wrapper over opaque type") {
    val m = Made.derived[OpaqueWrapper]
    val inner *: EmptyTuple = m.elems
    assertEquals(inner.apply(OpaqueWrapper(Ids("w-1"))), Ids("w-1"))
  }

  test("path-dependent field type via a fixed Outer") {
    val m = Made.derived[Holds]
    val inner *: EmptyTuple = m.elems
    assertEquals(inner.apply(Holds(Outer.shared.Inner("x"))).name, "x")
  }

  // --- Tuple-like shapes ---------------------------------------------------

  test("derives Product for plain tuple") {
    val m = Made.derived[(Int, String)]
    assertEquals(compiletime.constValueTuple[m.ElemLabels].toList, List("_1", "_2"))
    val v: (Int, String) = m.fromTuple((1, "x"))
    assertEquals(v, (1, "x"))
  }

  test("derives Product for named tuple") {
    val m = Made.derived[PersonNT]
    assertEquals(compiletime.constValueTuple[m.ElemLabels].toList, List("name", "age"))
    val nt: PersonNT = (name = "Alice", age = 30)
    val (n, a) = m.elems
    assertEquals(n.apply(nt), "Alice")
    assertEquals(a.apply(nt), 30)
  }

  test("empty case class") {
    val m = Made.derived[Empty]
    assertEquals(compiletime.constValueTuple[m.ElemLabels].toList, Nil)
    assertEquals(m.fromTuple(EmptyTuple), Empty())
  }

  test("derives the underlying Product for a refinement of a case class") {
    val m = Made.derived[Foo { val x: Int }]
    assertEquals(compiletime.constValueTuple[m.ElemLabels].toList, List("x"))
  }

  test("derives the underlying mirror through a type alias") {
    val m = Made.derived[AliasFoo]
    assertEquals(compiletime.constValueTuple[m.ElemLabels].toList, List("x"))
    val v: Foo = m.fromTuple(Tuple(7))
    assertEquals(v, Foo(7))
  }

  test("opaque-wrapped case class keeps the opaque label and derives via Mirror") {
    val m = OpaqueScope.deriveWrapped
    assertEquals(compiletime.constValueTuple[m.ElemLabels].toList, List("x"))
    // mirror.Label preserves the opaque alias name rather than dealiasing to Foo
    assertEquals(compiletime.constValue[m.Label], "WrappedFoo")
  }

  // --- Size / parameterisation ---------------------------------------------

  test("derives a case class with > 22 fields") {
    val m = Made.derived[Big]
    assertEquals(compiletime.constValueTuple[m.ElemLabels].toList.size, 25)
    val instance = Big(
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    )
    val a *: _ = m.elems
    assertEquals(a.apply(instance), 1)
  }

  test("derives a covariant generic ADT") {
    val m = Made.derived[Box[String]]
    val v *: EmptyTuple = m.elems
    assertEquals(v.label, "value")
    assertEquals(v.apply(Box("x")), "x")
  }

  test("derives a phantom-tagged wrapper") {
    val m = Made.derived[Tagged[String, UserTag]]
    val v *: EmptyTuple = m.elems
    assertEquals(v.apply(Tagged[String, UserTag]("u-1")), "u-1")
  }

  // --- Recursion / cycles --------------------------------------------------

  test("recursive product via List[Self]") {
    val m = Made.derived[Node]
    val v *: kids *: EmptyTuple = m.elems
    assertEquals(v.label, "value")
    assertEquals(kids.label, "children")
    val tree = Node(1, List(Node(2, Nil), Node(3, Nil)))
    assertEquals(v.apply(tree), 1)
    assertEquals(kids.apply(tree).size, 2)
  }

  test("derives a mutually recursive ADT") {
    val mA = Made.derived[NodeA]
    val mB = Made.derived[NodeB]
    assertEquals(compiletime.constValueTuple[mA.ElemLabels].toList, List("value", "next"))
    assertEquals(compiletime.constValueTuple[mB.ElemLabels].toList, List("label", "child"))
  }

  test("recursive ADT (linked list)") {
    val m = Made.derived[LList[Int]]
    assertEquals(m.ordinal(LList.Nil), 1)
    assertEquals(m.ordinal(LList.Cons(1, LList.Nil)), 0)
  }

  // --- Sum derivation ------------------------------------------------------

  test("derives Sum for GADT-style enum") {
    val m = Made.derived[Expr[Int]]
    assertEquals(m.ordinal(Expr.IntLit(1)), 0)
  }

  test("enum with a single case") {
    val m = Made.derived[Single]
    assertEquals(m.ordinal(Single.Only), 0)
  }

  test("enum cases sharing field names produce matching elem labels") {
    val m = Made.derived[Op]
    val (add, sub) = m.elems
    assertEquals(add.label, "Add")
    assertEquals(sub.label, "Sub")
  }

  test("case object extending two sealed parents shows up in both") {
    val mA = Made.derived[SubA]
    val mB = Made.derived[SubB]
    assertEquals(mA.ordinal(Multi), 0)
    assertEquals(mB.ordinal(Multi), 0)
  }

  // --- Visibility ----------------------------------------------------------

  test("private-constructor case class derives within its scope") {
    val m = PrivateScope.derive
    val x *: EmptyTuple = m.elems
    assertEquals(x.apply(PrivateScope.make(7)), 7)
  }

  // --- Metadata ------------------------------------------------------------

  test("annotation declared on parent trait is read on the case class field") {
    val m = Made.derived[WithInherited]
    val x *: EmptyTuple = m.elems
    assert(x.hasAnnotation[JsonName])
    assertEquals(x.getAnnotation[JsonName].value, "inherited")
  }

  // --- Negative-compile assertions -----------------------------------------

  test("union type as mirrored type is rejected") {
    val diags = SnippetCompiler.compile(
      // language=scala 3
      """import halotukozak.made.*
        |object S { val _ = Made.derived[Int | String] }
        |""".stripMargin,
    )
    assert(
      diags.containsMessage("Unsupported Mirror type"),
      s"expected 'Unsupported Mirror type' diagnostic but got: $diags",
    )
  }

  test("non-sealed trait without a Mirror is rejected") {
    val diags = SnippetCompiler.compile(
      // language=scala 3
      """import halotukozak.made.*
        |trait Open
        |object S { val _ = Made.derived[Open] }
        |""".stripMargin,
    )
    assert(
      diags.containsMessage("Unsupported Mirror type"),
      s"expected 'Unsupported Mirror type' diagnostic but got: $diags",
    )
  }

  test("multiple parameter lists are rejected by the compiler-provided Mirror") {
    val diags = SnippetCompiler.compile(
      // language=scala 3
      """import halotukozak.made.*
        |case class Multi(a: Int)(b: String)
        |object S { val _ = Made.derived[Multi] }
        |""".stripMargin,
    )
    assert(
      diags.containsMessage("Unsupported Mirror type") || diags.containsMessage("No given instance"),
      s"expected unsupported diagnostic but got: $diags",
    )
  }

  test("Made.derived requires a concrete type — type constructor List is rejected") {
    val diags = SnippetCompiler.compile(
      // language=scala 3
      """import halotukozak.made.*
        |object S { val _ = Made.derived[List] }
        |""".stripMargin,
    )
    assert(
      diags.containsMessage("Unsupported Mirror type") || diags.containsMessage("does not conform"),
      s"expected derivation rejection but got: $diags",
    )
  }

  // `derives Made` is not supported: `Made` carries `type Type` instead of a type parameter,
  // so the standard `T derives Made` desugaring does not apply. Users summon via `Made.Of[T]`
  // or `Made.derived[T]` directly.

object EdgeCaseTypesTest:
  // --- Opaque types --------------------------------------------------------
  object opaqueIds:
    opaque type Ids = String
    object Ids:
      def apply(s: String): Ids = s
  export opaqueIds.Ids

  case class WithOpaque(id: Ids, name: String)
  @transparent case class OpaqueWrapper(inner: Ids)

  // --- Compound field types ------------------------------------------------
  case class WithUnion(v: Int | String)

  trait MarkA
  trait MarkB
  case class WithIntersection(v: MarkA & MarkB)

  // --- Path-dependent -------------------------------------------------------
  class Outer:
    case class Inner(name: String)
  object Outer:
    val shared: Outer = new Outer
  case class Holds(inner: Outer.shared.Inner)

  // --- Tuple-shaped --------------------------------------------------------
  case class Empty()
  type PersonNT = (name: String, age: Int)

  case class Foo(x: Int)
  type AliasFoo = Foo

  object OpaqueScope:
    opaque type WrappedFoo = Foo
    object WrappedFoo:
      def apply(f: Foo): WrappedFoo = f
    // derive inside the opaque scope so Mirror.ProductOf[WrappedFoo] resolves
    def deriveWrapped = Made.derived[WrappedFoo]

  // --- Size / parameterisation ---------------------------------------------
  case class Big(
    a01: Int,
    a02: Int,
    a03: Int,
    a04: Int,
    a05: Int,
    a06: Int,
    a07: Int,
    a08: Int,
    a09: Int,
    a10: Int,
    a11: Int,
    a12: Int,
    a13: Int,
    a14: Int,
    a15: Int,
    a16: Int,
    a17: Int,
    a18: Int,
    a19: Int,
    a20: Int,
    a21: Int,
    a22: Int,
    a23: Int,
    a24: Int,
    a25: Int,
  )

  case class Box[+A](value: A)

  trait UserTag
  case class Tagged[A, Tag](value: A)

  // --- Recursive shapes ----------------------------------------------------
  case class Node(value: Int, children: List[Node])
  case class NodeA(value: Int, next: Option[NodeB])
  case class NodeB(label: String, child: Option[NodeA])

  enum LList[+A]:
    case Cons(head: A, tail: LList[A])
    case Nil extends LList[Nothing]

  // --- Sum / enum shapes ---------------------------------------------------
  enum Expr[T]:
    case IntLit(v: Int) extends Expr[Int]
    case StrLit(v: String) extends Expr[String]

  enum Single:
    case Only

  enum Op:
    case Add(lhs: Int, rhs: Int)
    case Sub(lhs: Int, rhs: Int)

  sealed trait SubA
  sealed trait SubB
  case object Multi extends SubA, SubB

  // --- Visibility ----------------------------------------------------------
  object PrivateScope:
    case class Hidden private[PrivateScope] (x: Int)
    def make(x: Int): Hidden = Hidden(x)
    def derive = Made.derived[Hidden]

  // --- Inherited annotations -----------------------------------------------
  class JsonName(val value: String) extends MetaAnnotation

  trait HasJson:
    @JsonName("inherited")
    def x: Int

  case class WithInherited(x: Int) extends HasJson
