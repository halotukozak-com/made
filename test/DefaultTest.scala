package halotukozak.made

class DefaultTest extends munit.FunSuite:
  test("Default for Option[A]") {
    val default = summon[Default[Option[String]]]

    assertEquals(default(), None)
  }

  test("Default for A | Null") {
    val default = summon[Default[String | Null]]

    assertEquals[Any, Any](default(), null: String | Null)
  }
