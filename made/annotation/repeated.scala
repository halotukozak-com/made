package halotukozak.made.annotation

/**
 * Marker injected by `Made.derived` on fields declared with varargs (`T*`).
 *
 * Users do not write `@repeated` on their case classes — the macro adds it
 * to the field's `Metadata` whenever the underlying parameter symbol carries
 * the `Repeated` flag. Consumers can query it via `hasAnnotation[repeated]`
 * to discover that the field originated from a varargs parameter, even
 * though the field type itself appears as `Seq[T]`.
 *
 * @see [[MetaAnnotation]]
 */
class repeated extends MetaAnnotation
