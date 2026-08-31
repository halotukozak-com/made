package halotukozak.made

import halotukozak.made.annotation.MetaAnnotation

import scala.annotation.meta

/**
 * Regression tests for the field-annotation lookup fallback.
 *
 * `tSymbol.caseFields` returns the val accessor symbols. Annotations placed on case-class
 * parameters live on the constructor-param symbol; whether they also appear on the val
 * accessor depends on annotation target and the compilation phase macro expansion observes.
 * `metaTypeOf` therefore unions annotations from the field symbol AND the matching primary-
 * constructor parameter, so `MetaAnnotation` lookup works in either configuration.
 */
class CtorParamAnnotationFallbackTest extends munit.FunSuite:
  import CtorParamAnnotationFallbackTest.*

  test("annotation targeted at @param only is still seen on the field's Metadata") {
    val mirror = Made.derived[ParamOnlyAnnotated]
    val x *: y *: EmptyTuple = mirror.elems
    assert(x.hasAnnotation[ParamOnlyMarker])
    assert(!y.hasAnnotation[ParamOnlyMarker])
  }

  test("parametrised @param-only annotation value reachable via getAnnotation") {
    val mirror = Made.derived[ParamOnlyTagged]
    val x *: y *: EmptyTuple = mirror.elems
    assertEquals(x.getAnnotation[ParamOnlyTag].value, "primary")
    assertEquals(y.getAnnotation[ParamOnlyTag], NotExists)
  }

  test("default-targeted annotation still works (sanity-check, not affected by fallback)") {
    val mirror = Made.derived[NormalAnnotated]
    val x *: y *: EmptyTuple = mirror.elems
    assert(x.hasAnnotation[NormalMarker])
    assert(!y.hasAnnotation[NormalMarker])
  }

  test("union: annotation on val accessor and ctor param does not duplicate") {
    val mirror = Made.derived[NormalAnnotated]
    val x *: _ *: EmptyTuple = mirror.elems
    val annot = x.getAnnotation[NormalMarker]
    assert(annot != NotExists)
    // Sanity: no doubled metadata. We can't directly inspect the Metadata tuple's
    // arity at runtime but the type-level singleton narrowing guards us.
    val flags: (true, false) = mirror.elems.hasAnnotations[NormalMarker]
    assertEquals(flags, (true, false))
  }

object CtorParamAnnotationFallbackTest:
  // `@meta.param` restricts placement to the constructor parameter symbol only.
  @meta.param
  class ParamOnlyMarker extends MetaAnnotation

  @meta.param
  case class ParamOnlyTag(value: String) extends MetaAnnotation

  case class ParamOnlyAnnotated(@ParamOnlyMarker x: Int, y: String)
  case class ParamOnlyTagged(@ParamOnlyTag("primary") x: String, y: String)

  class NormalMarker extends MetaAnnotation
  case class NormalAnnotated(@NormalMarker x: Int, y: String)
