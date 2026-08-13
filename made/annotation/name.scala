package halotukozak.made.annotation

import scala.annotation.RefiningAnnotation

/**
 * Overrides the `Label` type member for the annotated type or field.
 *
 * The `name` parameter becomes the compile-time string literal used as
 * `Label`. Can be applied to both types (overrides
 * `Made.Label`) and fields (overrides `MadeElem.Label`).
 *
 * @param name the label override
 * @see [[halotukozak.made.Made]]
 * @see [[halotukozak.made.MadeElem]]
 * @see [[MetaAnnotation]]
 */
class name(val name: String) extends RefiningAnnotation
