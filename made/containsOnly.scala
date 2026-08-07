package made

import made.containsOnly.refl

import scala.annotation.unchecked.uncheckedVariance as uv
import scala.annotation.unused

infix sealed trait containsOnly[-Tup <: Tuple, +T]:
  given (Tuple.Tail[Tup @uv] containsOnly T) = refl

  given (Tuple.Reverse[Tup @uv] containsOnly T) = refl

  given [Uup <: Tuple] => (@unused ev: Uup containsOnly T @uv) => (Tuple.Concat[Tup @uv, Uup] containsOnly T) = refl

  given [Uup <: Tuple, U] => (@unused ev: Uup containsOnly U) => (Tuple.Zip[Tup @uv, Uup] containsOnly (T, U)) = refl

object containsOnly extends containsOnlyLowPriority:

  type Loop[Tup <: Tuple, T] <: Boolean = Tup match
    case EmptyTuple => true
    case T *: tail => Loop[tail, T]
    case _ => false

  private val reusable = new containsOnly[Tuple, Nothing] {}

  def refl[Tup <: Tuple, T]: Tup containsOnly T = reusable

  given [Tup <: Tuple, T](using ev: Loop[Tup, T] =:= true): containsOnly[Tup, T] = refl

  /** A constant map `[_] =>> C` makes every element `C`. Unifies even for abstract `Es`. */
  given [Es <: Tuple, C] => (Tuple.Map[Es, [_] =>> C] containsOnly C) = refl

  /** A covariant `F` gives `F[e] <: F[Any]` for every element (invariant `F` still needs `refl`). */
  given [Es <: Tuple, F[+_]] => (Tuple.Map[Es, F] containsOnly F[Any]) = refl

  import scala.language.implicitConversions

  given [Tup <: Tuple, T] => (@unused ev: Tup containsOnly T) => Conversion[Tuple.Head[Tup], T] =
    _.asInstanceOf[T]

  given [Tup <: Tuple, T] => (@unused ev: Tup containsOnly T) => Conversion[Tuple.Last[Tup], T] =
    _.asInstanceOf[T]

sealed trait containsOnlyLowPriority:
  given Tuple containsOnly Any = containsOnly.refl
