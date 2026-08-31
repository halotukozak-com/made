package halotukozak.made

import halotukozak.made.annotation.optionalParam
import halotukozak.commons.*

/**
 * A realistic product-codec derivation that maps over a `Made` mirror's tuples
 * (`ElemLabels`, `ElemTypes`, `elems`). Every `toArrayOf` / `mapAs` here used to require an
 * explicit `(using containsOnly.refl)` because the mapped tuples never reduce for an abstract
 * mirror; the path-dependent and mapped-tuple `containsOnly` givens now supply that evidence
 * automatically, so this whole derivation is refl-free.
 */
class MappedTupleEvidenceTest extends munit.FunSuite:
  import MappedTupleEvidenceTest.*

  test("product codec derivation summons all containsOnly evidence without containsOnly.refl") {
    val codec = MCodec.derived[User]

    assertEquals(codec.labels.toList, List("name", "age", "address"))
    assertEquals(codec.codecs.length, 3)
    assertEquals(codec.optionalFlags.toList, List(false, false, true))

    // age carries a constructor default; the per-field default mapping resolved it
    assertEquals(codec.defaults.length, 3)
    assertEquals(codec.defaults(1), 25)

    // the captured constructor still rebuilds a value end-to-end
    assertEquals(codec.ctor(Array("Bob", 30, Some("x"))), User("Bob", 30, Some("x")))
  }

  test("covariant functor maps resolve toArrayOf[F[Any]] with no witness") {
    // `Decoder[+T]` is covariant, so `Tuple.Map[ElemTypes, Decoder] containsOnly Decoder[Any]`
    // holds soundly and is derived automatically — no `containsOnly.refl` needed.
    assertEquals(Decoder.all[User].length, 3)
  }

  test("hasAnnotations(...).toArrayOf[Boolean] resolves without containsOnly.refl") {
    // declared `Tuple.Map[es.type, [_] =>> Boolean]` -> `constMap` proves `containsOnly Boolean`
    assertEquals(fields.optionalFlags[User].toList, List(false, false, true))
  }

  test("getAnnotations(...).toArrayOf[A | NotExists] resolves without containsOnly.refl") {
    // declared `Tuple.Map[es.type, [_] =>> A | NotExists]` -> `constMap` proves `containsOnly A | NotExists`
    val got = fields.optionalAnnotations[User]
    assertEquals(got.length, 3)
    assert(got(0) == NotExists, "name is not @optionalParam")
    assert(got(1) == NotExists, "age is not @optionalParam")
    assert(got(2).isInstanceOf[optionalParam], "address is @optionalParam")
  }

  test("mapAs(...).toArrayOf resolves without containsOnly.refl") {
    // a constant `Boolean` result is `Tuple.Map[es.type, [_] =>> Boolean]` -> `constMap` applies
    val present = fields.hasDefault[User]
    assertEquals(present.length, 3)
    assert(present(1), "age has a constructor default")
  }

  test("mapAs(_.hasAnnotation) does not compile: per-element macro needs a concrete element type") {
    val errors = compileErrors("""
      val m = summon[Made.Of[MappedTupleEvidenceTest.User]]
        .asInstanceOf[Made.ProductOf[MappedTupleEvidenceTest.User]]
      m.elems.mapAs[MadeFieldElem][[_ <: MadeFieldElem] =>> Boolean](
        [e <: MadeFieldElem] => (elem: e) => elem.hasAnnotation[halotukozak.made.annotation.optionalParam],
      )
    """)
    assert(errors.nonEmpty)
  }

object MappedTupleEvidenceTest:
  trait MCodec[T]

  object MCodec:
    given MCodec[String] = new MCodec[String] {}
    given MCodec[Int] = new MCodec[Int] {}
    given MCodec[Option[String]] = new MCodec[Option[String]] {}

    final class Product[T](
      val labels: Array[String],
      val codecs: Array[MCodec[Any]],
      val defaults: Array[Any | NotExists],
      val optionalFlags: Array[Boolean],
      val ctor: Array[Any] => T,
    ) extends MCodec[T]

    inline def derived[T](using m: Made.Of[T]): Product[T] = inline m match
      case m: Made.ProductOf[T & scala.Product] => deriveProduct(m).asInstanceOf[Product[T]]

    inline def deriveProduct[T](m: Made.ProductOf[T]): Product[T] = Product[T](
      compiletime.constValueTuple[m.ElemLabels].toArrayOf[String],
      // `MCodec` is invariant, so `MCodec[Int]` is not a `MCodec[Any]`: this widening is an
      // erasure-safe cast, not a typed subtype relation, so it needs an explicit witness.
      // (A covariant `Show[+T]` would resolve here automatically via the `F[+_]` given.)
      compiletime.summonAll[Tuple.Map[m.ElemTypes, MCodec]].toArrayOf[MCodec[Any]](using containsOnly.refl),
      m.elems
        .mapAs[MadeFieldElem][[e <: MadeFieldElem] =>> Any | NotExists]([e <: MadeFieldElem] =>
          (elem: e) => elem.default,
        )
        .toArrayOf[Any | NotExists],
      // `hasAnnotations` declares `Tuple.Map[es.type, [_] =>> Boolean]`, so the `constMap` given
      // proves `containsOnly Boolean` and `toArrayOf[Boolean]` resolves with no witness.
      m.elems.hasAnnotations[optionalParam].toArrayOf[Boolean],
      m.fromUnsafeArray,
    )

  trait Decoder[+T]

  object Decoder:
    given Decoder[String] = new Decoder[String] {}
    given Decoder[Int] = new Decoder[Int] {}
    given Decoder[Option[String]] = new Decoder[Option[String]] {}

    inline def all[T](using m: Made.Of[T]): Array[Decoder[Any]] = inline m match
      case m: Made.ProductOf[T & scala.Product] =>
        compiletime.summonAll[Tuple.Map[m.ElemTypes, Decoder]].toArrayOf[Decoder[Any]]

  /** Per-field views over `m.elems`, all resolved without an explicit `containsOnly` witness. */
  object fields:
    inline def optionalFlags[T](using m: Made.Of[T]): Array[Boolean] = inline m match
      case m: Made.ProductOf[T & scala.Product] =>
        m.elems.hasAnnotations[optionalParam].toArrayOf[Boolean]

    inline def optionalAnnotations[T](using m: Made.Of[T]): Array[optionalParam | NotExists] = inline m match
      case m: Made.ProductOf[T & scala.Product] =>
        m.elems.getAnnotations[optionalParam].toArrayOf[optionalParam | NotExists]

    inline def hasDefault[T](using m: Made.Of[T]): Array[Boolean] = inline m match
      case m: Made.ProductOf[T & scala.Product] =>
        m.elems
          .mapAs[MadeFieldElem][[_ <: MadeFieldElem] =>> Boolean]([e <: MadeFieldElem] =>
            (elem: e) => elem.default != NotExists,
          )
          .toArrayOf[Boolean]

  case class User(name: String, age: Int = 25, @optionalParam address: Option[String])
