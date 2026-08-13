import scala.quoted.{Expr, Quotes, Type}

/**
 * Extended mirrors for Scala types, adding annotation metadata, element-level detail,
 * and generated member support on top of standard `scala.deriving.Mirror`.
 *
 * Derive a mirror with `Made.derived[T]`. The resulting mirror subtype depends on `T`:
 * singletons, transparent wrappers, products (case classes), or sums (sealed traits/enums).
 *
 * Each mirror carries an `Elems` tuple of [[made.MadeElem]] subtypes describing
 * constructor fields or sum subtypes, and a `Metadata` tuple of `Meta @ann` entries
 * encoding annotations applied to the mirrored type.
 *
 * @see [[made.Made]]
 * @see [[made.MadeElem]]
 */
package object made:

  inline def raiseUnsupportedTypeFor[For <: AnyKind, Provided] = ${
    raiseUnsupportedTypeForImpl[For, Provided]
  }

  private def raiseUnsupportedTypeForImpl[For <: AnyKind: Type, Provided: Type](using quotes: Quotes): Expr[Nothing] =
    import quotes.reflect.*
    given Printer[TypeRepr] = Printer.TypeReprShortCode

    report.error(s"Unsupported type for ${TypeRepr.of[For].show}: ${TypeRepr.of[Provided].show}")
    '{ ??? }

  inline def raiseCannotDerivedTypeFor[For <: AnyKind, Provided] = ${
    raiseCannotDerivedTypeForImpl[For, Provided]
  }

  private def raiseCannotDerivedTypeForImpl[For <: AnyKind: Type, Provided: Type](using quotes: Quotes): Expr[Nothing] =
    import quotes.reflect.*
    given Printer[TypeRepr] = Printer.TypeReprShortCode

    report.error(s"Cannot derive for ${TypeRepr.of[For].show} for ${TypeRepr.of[Provided].show}")
    '{ ??? }
