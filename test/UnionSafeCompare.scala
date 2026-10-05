package halotukozak.made

/**
 * munit's own Compare instances take an `A <:< B` given, and resolving such a given for a union type
 * trips -Winfer-union even when the union comes from the expected type rather than from inference.
 * These instances express the same subtype relation through bounds, which doesn't warn; the subtype
 * one is in the subclass so that it wins when both apply.
 */
trait SupertypeCompare:
  given supertypeCompare[A, B <: A]: munit.Compare[A, B] = munit.Compare.defaultCompare

trait UnionSafeCompare extends SupertypeCompare:
  given subtypeCompare[A <: B, B]: munit.Compare[A, B] = munit.Compare.defaultCompare
