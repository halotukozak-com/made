package made
package tuple

class MapTupleTest extends munit.FunSuite:

  test("increments literal int elements and narrows result types") {
    val tup: (1, 2, 3) = (1, 2, 3)
    val res: (2, 3, 4) = mapTuple(tup)[Int]([i <: Int] => i => i + 1)
    assertEquals(res, (2, 3, 4))
  }

  test("empty tuple") {
    val res: EmptyTuple = mapTuple(EmptyTuple)[Any]([e <: Any] => e => e)
    assertEquals(res, EmptyTuple)
  }

  test("single element tuple") {
    val tup: Tuple1[5] = Tuple1(5)
    val res: Tuple1[6] = mapTuple(tup)[Int]([i <: Int] => i => i + 1)
    assertEquals(res, Tuple1(6))
  }

  test("multiplies literal elements, param used twice") {
    val tup: (2, 3, 4) = (2, 3, 4)
    val res: (4, 9, 16) = mapTuple(tup)[Int]([i <: Int] => i => i * i)
    assertEquals(res, (4, 9, 16))
  }

  test("subtracts literal elements") {
    val tup: (5, 10, 15) = (5, 10, 15)
    val res: (4, 9, 14) = mapTuple(tup)[Int]([i <: Int] => i => i - 1)
    assertEquals(res, (4, 9, 14))
  }

  test("works on non-literal Int tuple") {
    val tup: (Int, Int, Int) = (10, 20, 30)
    val res: (Int, Int, Int) = mapTuple(tup)[Int]([i <: Int] => i => i + 1)
    assertEquals(res, (11, 21, 31))
  }

  test("works on String elements") {
    val tup: (String, String) = ("foo", "bar")
    val res: (String, String) = mapTuple(tup)[String]([s <: String] => s => s + "!")
    assertEquals(res, ("foo!", "bar!"))
  }

  test("preserves element order") {
    val tup: ("a", "b", "c") = ("a", "b", "c")
    val res: (String, String, String) = mapTuple(tup)[String]([s <: String] => s => s + s)
    assertEquals(res, ("aa", "bb", "cc"))
  }

  test("body may reference captured outer state") {
    var count = 0
    def bump(x: Int): Int = { count += 1; x }
    val tup: (1, 2, 3) = (1, 2, 3)
    mapTuple(tup)[Int]([i <: Int] => i => bump(i))
    assertEquals(count, 3)
  }

  test("body may call an outer helper function") {
    def double(x: Int): Int = x * 2
    val tup: (1, 2, 3) = (1, 2, 3)
    val res: (Int, Int, Int) = mapTuple(tup)[Int]([i <: Int] => i => double(i))
    assertEquals(res, (2, 4, 6))
  }

  test("body may branch with a conditional") {
    val tup: (1, 2, 3) = (1, 2, 3)
    val res: (String, String, String) =
      mapTuple(tup)[Int]([i <: Int] => i => if i == 1 then "one" else i.toString)
    assertEquals(res, ("one", "2", "3"))
  }

  test("works with U = Any over a heterogeneous tuple") {
    val tup: (Int, String, Boolean) = (1, "two", true)
    val res: (String, String, String) = mapTuple(tup)[Any]([e <: Any] => e => e.toString)
    assertEquals(res, ("1", "two", "true"))
  }

  test("works on a derived tuple from tail") {
    val tup: (1, 2, 3, 4) = (1, 2, 3, 4)
    val res: (3, 4, 5) = mapTuple(tup.tail)[Int]([i <: Int] => i => i + 1)
    assertEquals(res, (3, 4, 5))
  }

  test("works on a large tuple") {
    val tup: (1, 2, 3, 4, 5, 6) = (1, 2, 3, 4, 5, 6)
    val res: (Int, Int, Int, Int, Int, Int) = mapTuple(tup)[Int]([i <: Int] => i => i * 10)
    assertEquals(res, (10, 20, 30, 40, 50, 60))
  }

  // Regression: the lambda's own type parameter (`t` in `[t <: Int] => ...`) can leak into the
  // body via type inference on generic constructors (e.g. `List.apply[t]`), not just via the
  // value parameter. Both the term and the type occurrence must be substituted per index.
  test("body wraps the element in a generic constructor (List)") {
    val tup: (1, 2, 3) = (1, 2, 3)
    val res: (List[Int], List[Int], List[Int]) = mapTuple(tup)[Int]([i <: Int] => i => List(i))
    assertEquals(res, (List(1), List(2), List(3)))
  }

  test("body wraps the element in a generic constructor (Option)") {
    val tup: ("a", "b") = ("a", "b")
    val res: (Option[String], Option[String]) = mapTuple(tup)[String]([s <: String] => s => Option(s))
    assertEquals(res, (Some("a"), Some("b")))
  }

  test("body wraps the element in a generic constructor (Some)") {
    val tup: (1, 2) = (1, 2)
    val res: (Some[Int], Some[Int]) = mapTuple(tup)[Int]([i <: Int] => i => Some(i))
    assertEquals(res, (Some(1), Some(2)))
  }

  test("body wraps the element in a generic constructor (Set)") {
    // Set is invariant, so the result keeps the precise literal element types (unlike the
    // covariant List/Option/Some cases above, which widen to List[Int]/Option[String]/Some[Int]).
    val tup: (1, 2) = (1, 2)
    val res: (Set[1], Set[2]) = mapTuple(tup)[Int]([i <: Int] => i => Set(i))
    assertEquals(res, (Set[1](1), Set[2](2)))
  }

  test("body passes the element to a varargs generic constructor twice") {
    val tup: (1, 2) = (1, 2)
    val res: (List[Int], List[Int]) = mapTuple(tup)[Int]([i <: Int] => i => List(i, i))
    assertEquals(res, (List(1, 1), List(2, 2)))
  }
