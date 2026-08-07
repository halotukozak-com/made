package made

import made.containsOnly.given
import made.util.assumeExists

import scala.language.implicitConversions

class ContainsOnlyTest extends munit.FunSuite:

  // --- Loop type-level: positive cases ---

  test("Loop: EmptyTuple containsOnly Int") {
    summon[containsOnly.Loop[EmptyTuple, Int] =:= true]
  }

  test("Loop: EmptyTuple containsOnly String") {
    summon[containsOnly.Loop[EmptyTuple, String] =:= true]
  }

  test("Loop: single Int containsOnly Int") {
    summon[containsOnly.Loop[Tuple1[Int], Int] =:= true]
  }

  test("Loop: single String containsOnly String") {
    summon[containsOnly.Loop[Tuple1[String], String] =:= true]
  }

  test("Loop: (Int, Int) containsOnly Int") {
    summon[containsOnly.Loop[(Int, Int), Int] =:= true]
  }

  test("Loop: (Int, Int, Int) containsOnly Int") {
    summon[containsOnly.Loop[(Int, Int, Int), Int] =:= true]
  }

  test("Loop: (String, String) containsOnly String") {
    summon[containsOnly.Loop[(String, String), String] =:= true]
  }

  test("Loop: (Boolean, Boolean, Boolean) containsOnly Boolean") {
    summon[containsOnly.Loop[(Boolean, Boolean, Boolean), Boolean] =:= true]
  }

  test("Loop: (Double, Double) containsOnly Double") {
    summon[containsOnly.Loop[(Double, Double), Double] =:= true]
  }

  // --- Loop type-level: negative cases ---

  test("Loop: (Int, String) does not containOnly Int") {
    summon[containsOnly.Loop[(Int, String), Int] =:= false]
  }

  test("Loop: (String, Int) does not containOnly String") {
    summon[containsOnly.Loop[(String, Int), String] =:= false]
  }

  test("Loop: (Int, Int, String) does not containOnly Int") {
    summon[containsOnly.Loop[(Int, Int, String), Int] =:= false]
  }

  test("Loop: (String, Int, String) does not containOnly String") {
    summon[containsOnly.Loop[(String, Int, String), String] =:= false]
  }

  test("Loop: (Int, Double) does not containOnly Int") {
    summon[containsOnly.Loop[(Int, Double), Int] =:= false]
  }

  test("Loop: single Int does not containOnly String") {
    summon[containsOnly.Loop[Tuple1[Int], String] =:= false]
  }

  // --- Loop type-level: subtype behavior ---

  test("Loop: subtypes with sealed trait") {
    sealed trait Fruit
    case class Apple() extends Fruit
    case class Banana() extends Fruit

    summon[containsOnly.Loop[(Apple, Banana), Fruit] =:= true]
  }

  test("Loop: single subtype with sealed trait") {
    sealed trait Fruit
    case class Apple() extends Fruit

    summon[containsOnly.Loop[Tuple1[Apple], Fruit] =:= true]
  }

  test("Loop: mixed subtype and unrelated type") {
    sealed trait Fruit
    case class Apple() extends Fruit

    summon[containsOnly.Loop[(Apple, String), Fruit] =:= false]
  }

  // --- given instance: evidence is summoned correctly ---

  test("given: evidence available for EmptyTuple") {
    summon[EmptyTuple containsOnly Int]
  }

  test("given: evidence available for (Int, Int, Int)") {
    summon[(Int, Int, Int) containsOnly Int]
  }

  test("given: evidence available for (String, String)") {
    summon[(String, String) containsOnly String]
  }

  test("given: evidence available for single element") {
    summon[Tuple1[Int] containsOnly Int]
  }

  test("given: no evidence for heterogeneous tuple") {
    val errors = compileErrors("summon[(Int, String) containsOnly Int]")
    assert(errors.nonEmpty, "Should not find evidence for heterogeneous tuple")
  }

  test("given: no evidence for wrong type") {
    val errors = compileErrors("summon[(Int, Int) containsOnly String]")
    assert(errors.nonEmpty, "Should not find evidence for wrong type")
  }

  // --- integration with mapAs ---

  test("mapAs compiles for homogeneous Int tuple") {
    val result = (1, 2, 3).mapAs[Int]([t <: Int] => (x: t) => List(x))
    assertEquals(result, (List(1), List(2), List(3)))
  }

  test("mapAs compiles for homogeneous String tuple") {
    val result = ("a", "b").mapAs[String]([t <: String] => (x: t) => Option(x))
    assertEquals(result, (Some("a"), Some("b")))
  }

  test("mapAs compiles for empty tuple") {
    val result = EmptyTuple.mapAs[Int]([t <: Int] => (x: t) => List(x))
    assertEquals(result, EmptyTuple)
  }

  test("mapAs compiles for single element") {
    val result = Tuple1(42).mapAs[Int]([t <: Int] => (x: t) => Option(x))
    assertEquals(result, Tuple1(Some(42)))
  }

  test("mapAs compiles for subtype hierarchy") {
    sealed trait Animal
    case class Dog(name: String) extends Animal
    case class Cat(name: String) extends Animal

    val result = (Dog("Rex"), Cat("Whiskers")).mapAs[Animal]([t <: Animal] => (x: t) => Option(x))
    assertEquals(result, (Some(Dog("Rex")), Some(Cat("Whiskers"))))
  }

  // --- does not compile cases ---

  test("does not compile: heterogeneous tuple with mapAs") {
    val errors = compileErrors("""
      (1, "a").mapAs[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: wrong target type with mapAs") {
    val errors = compileErrors("""
      (1, 2, 3).mapAs[String]([t <: String] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: Int and Double mix") {
    val errors = compileErrors("""
      (1, 2.0).mapAs[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: Boolean among Ints") {
    val errors = compileErrors("""
      (1, true, 2).mapAs[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: String among Ints at end") {
    val errors = compileErrors("""
      (1, 2, "three").mapAs[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: String among Ints at start") {
    val errors = compileErrors("""
      ("zero", 1, 2).mapAs[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("evidence allows head access") {
    val tuple: Tuple = (1, 2, 3)
    given tuple.type containsOnly Int = containsOnly.refl

    val first: Int = tuple.head
    assertEquals(first, 1)
  }

  test("evidence allows last access") {
    val tuple: Tuple = (1, 2, 3)
    given tuple.type containsOnly Int = containsOnly.refl

    val last: Int = tuple.last
    assertEquals(last, 3)
  }

  // `containsOnly` only provides Head/Last → T conversions today. Operations like
  // `apply(i)`, `drop`, `take`, `tail`, `init`, `reverse`, `mapAs`, `toList`, `++`
  // need the static tuple shape to reduce match types, which we don't carry on an
  // abstract `Tuple` value. Coverage for those would require a richer evidence type.

  test("evidence on single element") {
    val tuple: Tuple = Tuple1(42)
    given tuple.type containsOnly Int = containsOnly.refl

    val first: Int = tuple.head
    assertEquals(first, 42)
  }

  test("evidence with String tuple") {
    val tuple: Tuple = ("a", "b", "c")
    given tuple.type containsOnly String = containsOnly.refl

    val first: String = tuple.head
    assertEquals(first, "a")
    assertEquals(tuple.size, 3)
  }

  test("containsOnly is contravariant in Tup") {
    sealed trait Fruit

    class Apple extends Fruit

    class Banana extends Fruit

    summon[(Fruit, Fruit) containsOnly Fruit]
    // Since (Apple, Apple) <: (Fruit, Fruit)
    // and containsOnly is -Tup
    // then containsOnly[(Fruit, Fruit), Fruit] <: containsOnly[(Apple, Apple), Fruit]
    summon[(Apple, Apple) containsOnly Fruit]

    type Super
    type A <: Super
    type B <: Super

    summon[(A, A) containsOnly Super]
    summon[(A, B) containsOnly Super]
  }

  test("containsOnly is covariant in T: singleton widens to Int") {
    val tuple: Tuple = (3, 3, 3)
    given (tuple.type containsOnly 3) = containsOnly.refl

    assumeExists[tuple.type containsOnly Int]
  }

  test("containsOnly is covariant in T: subclass widens to superclass") {
    sealed trait Fruit
    class Apple extends Fruit

    val tuple: Tuple = ("a", "b")

    given (tuple.type containsOnly Apple) = containsOnly.refl

    assumeExists[tuple.type containsOnly Fruit]
  }

  test("containsOnly is covariant in T: abstract type bound widens") {
    type Super
    type A <: Super

    val tuple: Tuple = ("a", "b")

    given (tuple.type containsOnly A) = containsOnly.refl

    assumeExists[tuple.type containsOnly Super]
  }

  test("containsOnly is covariant in T: String literal widens to String") {
    val tuple: Tuple = ("a", "b")
    given (tuple.type containsOnly "a") = containsOnly.refl

    assumeExists[tuple.type containsOnly String]
  }

  test("any tuple containsOnly Any without explicit evidence") {
    val tuple = ("a", "b")

    assumeExists[tuple.type containsOnly Any]
  }

  test("heterogeneous tuple containsOnly Any") {
    assumeExists[(Int, String, Boolean) containsOnly Any]
  }

  test("abstract Tuple containsOnly Any") {
    val tuple: Tuple = (1, "x", true)

    assumeExists[tuple.type containsOnly Any]
  }

  test("EmptyTuple containsOnly Any") {
    assumeExists[EmptyTuple containsOnly Any]
  }

  test("nested tuple containsOnly Any") {
    assumeExists[((Int, Int), (String, String)) containsOnly Any]
  }

  test("Tuple.Concat preserves containsOnly") {
    val tuple1 = ("one", "two", "three")
    val tuple2 = ("four", "five")

    assumeExists[Tuple.Concat[tuple1.type, tuple2.type] containsOnly String]

    import Tuple.++
    assumeExists[(tuple1.type ++ tuple2.type) containsOnly String]
  }

  test("Tuple.Tail preserves containsOnly") {
    val tuple = ("one", "two", "three")

    assumeExists[Tuple.Tail[tuple.type] containsOnly String]
  }

  test("Tuple.Reverse preserves containsOnly") {
    val tuple = ("one", "two", "three")

    assumeExists[Tuple.Reverse[tuple.type] containsOnly String]
  }
