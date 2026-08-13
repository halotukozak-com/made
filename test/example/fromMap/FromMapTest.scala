package halotukozak.made
package example.fromMap

import halotukozak.made.annotation.optionalParam

class FromMapTest extends munit.FunSuite:
  case class User(name: String, age: Int = 25, @optionalParam address: Option[String]) derives FromMap

  test("case class") {
    val alice = summon[FromMap[User]].fromMap(Map("name" -> "Alice", "age" -> 30))
    assert(alice == User("Alice", 30, None))

    val bob = summon[FromMap[User]].fromMap(Map("name" -> "Bob", "address" -> Some("Cracow")))
    assert(bob == User("Bob", 25, Some("Cracow")))
  }

  type Bill = (amount: Double, tip: Double)

  test("named tuple") {
    given FromMap[Bill] = FromMap.derived
    val bill: Bill = summon[FromMap[Bill]].fromMap(Map("amount" -> 67.0, "tip" -> 0.1))
    val expected: Bill = (amount = 67.0, tip = 0.1)
    assert(bill == expected)
  }

  // Annotations on named-tuple fields (e.g. `@whenAbsent(0.1) tip: Double`) are not yet
  // parseable in Scala 3 — `@` is rejected between the leading `(` and the field identifier.
  // Re-add a `@whenAbsent` case here once the syntax is supported and the deriver carries
  // per-field metadata through the symbol-less product path.
