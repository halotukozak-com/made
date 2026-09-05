# M&DE

**Mirror Annotations & Default Extraction** — a Scala 3 macro library that extends `scala.deriving.Mirror` with
annotation metadata, default values, generated members, and transparent wrapper support.

## Overview

Scala 3's built-in `Mirror` provides basic type-level information about case classes and enums, but it stops short of
exposing annotations, default values, or computed members. M&DE fills that gap — it derives enriched mirrors at
compile time that carry:

- **Type-level annotation metadata** — custom annotations on types and fields, queryable at both type level and runtime
- **Default value extraction** — from constructor defaults, `@whenAbsent` annotations, and `@optionalParam` markers
- **Generated members** — `@generated` vals and defs exposed as first-class elements of the mirror
- **Transparent wrappers** — `@transparent` single-field case classes with compile-time wrap/unwrap
- **Custom labels** — `@name` to override the label of a type or field

M&DE supports case classes, enums, sealed traits, objects, value classes, and higher-kinded types.

## Installation

M&DE is published to Maven Central under `com.halotukozak`. Requires Scala 3.

### scala-cli

```scala
//> using scala 3.9.0
//> using dep com.halotukozak::made::0.6.0
```

### sbt

```scala
scalaVersion := "3.9.0"
libraryDependencies += "com.halotukozak" %% "made" % "0.6.0"
```

### mill

```scala
def scalaVersion = "3.9.0"
def mvnDeps = Seq(mvn"com.halotukozak::made::0.4.1")
```

## Quickstart

Derive a `Made` mirror for a case class and inspect its fields, labels, and annotations.

```scala
import halotukozak.made.*
import halotukozak.made.annotation.*

case class User(@name("user_name") name: String, age: Int = 18)

val mirror = Made.derived[User]

// Type-level
//   mirror.Label       =:= "User"
//   mirror.ElemLabels  =:= ("user_name", "age")
//   mirror.ElemTypes   =:= (String, Int)

// Runtime
val (nameElem, ageElem) = mirror.elems
nameElem.label  // "user_name"
ageElem.default // 18 (NotExists if the field had none)

// Build values
val u = mirror.fromUnsafeArray(Array("Alice", 30))
```

See the guides for full coverage of deriving type classes, default extraction, generated members, and transparent
wrapping.

## Acknowledgements

M&DE is inspired by:

- [**AVSystem commons**](https://github.com/AVSystem/scala-commons) by [**ghik**](https://github.com/ghik)
- [**ops-mirror**](https://github.com/bishabosha/ops-mirror) by [**bishabosha**](https://github.com/bishabosha)
