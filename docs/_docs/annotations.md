---
title: Annotations and Metadata
order: 5
---

# Annotations and Metadata

This guide covers how Made surfaces annotations through `Metadata`, how to define your own annotations via
`MetaAnnotation`, how to rename types and fields with `@name`, and how to compose annotations with
`AnnotationAggregate`. The guide assumes familiarity with the basic Made workflow from
[Deriving Type Classes with Made](deriving-show.md).

## The Metadata Type Member

Every `Made` mirror exposes a `type Metadata <: Tuple` carrying annotations declared on the mirrored type. Per-element
metadata is carried by each `MadeElem` under the same `Metadata` type member. When no Made-aware annotations are
present, `Metadata =:= EmptyTuple`.

```scala
import made.*
import made.annotation.*

class JsonName(val value: String) extends MetaAnnotation

@JsonName("user")
case class User(@JsonName("user_name") name: String)

val mirror = Made.derived[User]

mirror.hasAnnotation[JsonName]                     // true
mirror.getAnnotation[JsonName].value               // "user"

val name *: EmptyTuple = mirror.elems
name.hasAnnotation[JsonName]                       // true
name.getAnnotation[JsonName].value                 // "user_name"
```

`hasAnnotation` and `getAnnotation` are `transparent inline` macros — they resolve against the type-level `Metadata`
tuple, so the result type is precise and the call has no reflective cost at runtime. `getAnnotation[A]` returns
`A | Null`: when the macro can already tell the annotation is present (as above, since `User` and its `name` field
are annotated literally), the result narrows all the way to `A` and `.value` is available directly, with no
`.get`/`.map` unwrapping.

When presence isn't known until the call site sees a concrete field — e.g. inside code that's generic over which
element it's looking at — the result narrows to `A | Null` instead, and a plain `!= null` check flow-types it back
to `A`:

```scala
val annot: JsonName | Null = name.getAnnotation[JsonName]
if annot != null then annot.value else "unnamed"      // "user_name" — no Option, no .get
```

This mirrors `made.Default`'s existing `A | Null` convention for "optional" values rather than introducing a second
shape alongside `Option`. `getAllAnnotations[A]`, which collects every annotation of type `A` on an element, stays
`List[A]`, since a list already has a natural empty case.

## Defining Custom Annotations

Annotations extending `MetaAnnotation` participate in Made's metadata. `MetaAnnotation` extends
`scala.annotation.RefiningAnnotation`, so each annotation refines the type of the annotated element and survives
into the mirror's `Metadata` tuple.

```scala
import made.annotation.MetaAnnotation

class JsonName(val value: String) extends MetaAnnotation
class Optional extends MetaAnnotation
class MinLength(val value: Int) extends MetaAnnotation
```

Annotations that do not extend `MetaAnnotation` are ignored by the metadata system. Standard library annotations such
as `@deprecated` will not appear in `Metadata`.

## Renaming with `@name`

`@name(literal)` overrides the `Label` of a type or field. The override is reflected in `mirror.Label`,
`mirror.ElemLabels`, and the runtime `MadeElem#label` accessor.

```scala
import made.*
import made.annotation.name

@name("user")
case class User(@name("user_name") name: String, age: Int)

val mirror = Made.derived[User]

compiletime.constValue[mirror.Label]               // "user"
compiletime.constValueTuple[mirror.ElemLabels]     // ("user_name", "age")

val (n, a) = mirror.elems
n.label                                            // "user_name"
a.label                                            // "age"
```

`@name` is declared as `extends RefiningAnnotation`, not `MetaAnnotation` — it is read by the deriver directly to
shape `Label`, so it does not show up under `Metadata`. Use a separate `MetaAnnotation` if you need both a label
override and a queryable annotation instance.

## Composing Annotations with `AnnotationAggregate`

An `AnnotationAggregate` is an "annotation function" — applying it to a symbol behaves as if all the annotations
declared on its `aggregated` method were applied directly. This lets you bundle a recurring set of annotations behind
a single name.

To define an aggregate, extend `AnnotationAggregate` and implement `aggregated` with the `reifyAggregated` macro.
The implementation must be `final def`, and the aggregated annotations are declared on that method.

```scala
import made.annotation.*
import scala.annotation.StaticAnnotation

class mongoId extends AnnotationAggregate:
  @name("_id")
  final def aggregated: List[StaticAnnotation] = reifyAggregated

case class Doc(@mongoId id: String, data: String)
```

Applying `@mongoId` to `id` behaves as if `@name("_id")` had been applied directly. The deriver expands aggregates
during metadata collection — `mirror.elems.head.label` is `"_id"`.

Constructor parameters of the aggregate can be referenced in inner annotation arguments and are substituted at the
application site:

```scala
import made.annotation.*
import scala.annotation.StaticAnnotation

class customName(n: String) extends AnnotationAggregate:
  @name(n)
  final def aggregated: List[StaticAnnotation] = reifyAggregated

case class Field(@customName("custom_name") x: Int)
```

Aggregates compose freely with `MetaAnnotation` — query `hasAnnotation` / `getAnnotation` against the inner
annotation type, not the aggregate:

```scala
import made.*
import made.annotation.*
import scala.annotation.StaticAnnotation

class InnerMeta extends MetaAnnotation

class withMeta extends AnnotationAggregate:
  @InnerMeta
  final def aggregated: List[StaticAnnotation] = reifyAggregated

case class Carried(@withMeta x: Int)

val m = Made.derived[Carried]
val x *: EmptyTuple = m.elems
x.hasAnnotation[InnerMeta]                         // true
x.getAnnotation[InnerMeta] != null                 // true
```

### Constraints

- The `aggregated` method must be a `final def` implemented via `reifyAggregated`. Other shapes are rejected at
  macro expansion.
- `reifyAggregated` collects only annotations whose type is a subtype of `StaticAnnotation`. Annotations from outside
  the standard annotation hierarchy are skipped.
