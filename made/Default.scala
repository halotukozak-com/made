package halotukozak.made

/**
 * Type class providing a default "empty" value for optional types.
 *
 * Used by `@optionalParam` to supply a default when no explicit default is provided.
 * Instances are defined for `Option[A]` (returns `None`) and `A | Null` (returns `null`).
 *
 * This is distinct from [[NotExists]], which marks the *absence* of any default at all
 * (see [[MadeFieldElem.default]]); `Default` only produces the "empty" value for a field that
 * has opted into an optional type.
 *
 * @tparam O the type for which a default value is provided
 * @see [[halotukozak.made.annotation.optionalParam]]
 * @see [[MadeFieldElem]]
 */
trait Default[O] extends (() => O)

object Default:
  given [A] => Default[Option[A]] = () => None
  given [A <: AnyRef] => Default[A | Null] = () => null
