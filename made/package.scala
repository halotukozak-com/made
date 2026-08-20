package halotukozak

/**
 * Extended mirrors for Scala types, adding annotation metadata, element-level detail,
 * and generated member support on top of standard `scala.deriving.Mirror`.
 *
 * Derive a mirror with `Made.derived[T]`. The resulting mirror subtype depends on `T`:
 * singletons, transparent wrappers, products (case classes), or sums (sealed traits/enums).
 *
 * Each mirror carries an `Elems` tuple of [[halotukozak.made.MadeElem]] subtypes describing
 * constructor fields or sum subtypes, and a `Metadata` tuple of `Meta @ann` entries
 * encoding annotations applied to the mirrored type.
 *
 * @see [[halotukozak.made.Made]]
 * @see [[halotukozak.made.MadeElem]]
 */
package object made