package halotukozak.made
package example.show

class ShowTest extends munit.FunSuite:
  test("showProduct") {
    val userShow = Show.derived[User]

    assertEquals(userShow.show(User("Alice", 30)), "User(name = Alice, age = 30)")
  }

  test("showTransparent") {
    val emailShow = Show.derived[Email]

    assertEquals(emailShow.show(Email("alice@example.com")), "alice@example.com")
  }

  test("showSum") {
    given Show[Circle] = Show.derived[Circle]
    given Show[Rectangle] = Show.derived[Rectangle]
    given Show[Point.type] = Show.derived[Point.type]

    val shapeShow: Show[Shape] = Show.derived[Shape]

    assertEquals(shapeShow.show(Point), "Point")
    assertEquals(shapeShow.show(Circle(3.14)), "Circle(radius = 3.14)")
    assertEquals(shapeShow.show(Rectangle(2.0, 5.0)), "Rectangle(width = 2.0, height = 5.0)")
  }

  test("showSingleton") {
    val originShow = Show.derived[Origin.type]
    val unitShow = Show.derived[Unit]

    assertEquals(originShow.show(Origin), "Origin")
    assertEquals(unitShow.show(()), "Unit")
  }
