package made
package example.fromMap

import made.annotation.optionalParam

class FromMapTest extends munit.FunSuite:
  case class User(name: String, age: Int = 25, @optionalParam address: Option[String]) derives FromMap

  test("case class") {
    val alice = summon[FromMap[User]].fromMap(Map("name" -> "Alice", "age" -> 30))
    assert(alice == User("Alice", 30, None))

    val bob = summon[FromMap[User]].fromMap(Map("name" -> "Bob", "address" -> Some("Cracow")))
    assert(bob == User("Bob", 25, Some("Cracow")))
  }

  // Named-tuple coverage (e.g. `type Bill = (amount: Double, @whenAbsent(0.1) tip: Double)`)
  // is blocked by the named-tuple deriver gap tracked in EdgeCaseTypesTest.
