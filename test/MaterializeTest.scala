package made

class MaterializeTest extends munit.FunSuite:

  trait Calc:
    def add(a: Int, b: Int): Int
    def name: String
    def ping(): Boolean

  trait Props:
    def magic: Int
    def message: String

  test("todo"):
    val handlers = (
      () => "calc",
      () => true,
    )
    val c: Calc = handlers.to[Calc]
    assertEquals(c.add(2, 3), 5)
    assertEquals(c.name, "calc")
    assertEquals(c.ping(), true)

  test("materialize synthesizes a working instance from a tuple of per-op handlers"):
    val handlers = (
      (args: (a: Int, b: Int)) => args.a + args.b,
      () => "calc",
      () => true,
    )
    val c: Calc = handlers.to[Calc]
    assertEquals(c.add(2, 3), 5)
    assertEquals(c.name, "calc")
    assertEquals(c.ping(), true)

  test("materialize supports def-only traits"):
    val handlers = (
      () => 42,
      () => "hello",
    )
    val v: Props = handlers.to[Props]
    assertEquals(v.magic, 42)
    assertEquals(v.message, "hello")

  test("materialize handlers are called on every invocation, not cached"):
    var count = 0
    val handlers = (
      () =>
        count += 1
        count
      ,
      () => "x",
    )
    val v: Props = handlers.to[Props]
    assertEquals(v.magic, 1)
    assertEquals(v.magic, 2)
    assertEquals(v.magic, 3)

  test("materialize supports multi-param-list methods"):
    val handler = (args: (left: String, right: Int)) => args.left + args.right.toString
    val svc: MultiListService = (handler *: EmptyTuple).to[MultiListService]
    assertEquals(svc.combine("x")(3), "x3")

  test("materialize supports Unit-returning side-effectful methods"):
    val log = collection.mutable.ArrayBuffer.empty[String]
    val handlers = (
      (args: (msg: String)) => log.append(args.msg),
      () => log.append("tick"),
    )
    val u: UnitReturning = handlers.to[UnitReturning]
    u.log("hello")
    u.tick
    assertEquals(log.toList, List("hello", "tick"))

  test("materialize supports generic types"):
    val handler = (args: (raw: String)) => args.raw.toInt
    val conv: Converter[Int] = (handler *: EmptyTuple).to[Converter[Int]]
    assertEquals(conv.convert("42"), 42)

  trait Base:
    def fromBase: String

  trait Extended extends Base:
    def own: Int

  test("materialize includes inherited methods in source order"):
    val handlers = (
      () => "from-base",
      () => 99,
    )
    val v: Extended = handlers.to[Extended]
    assertEquals(v.fromBase, "from-base")
    assertEquals(v.own, 99)

