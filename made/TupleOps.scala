package made

import scala.annotation.unused
import scala.compiletime.ops.boolean.||
import scala.compiletime.ops.int.S
import scala.reflect.ClassTag

extension (tup: Tuple)
  inline def foreach(f: [t] => t => Unit): Unit = tup.map[[X] =>> Unit](f)

  inline def indices: Indices[tup.type] = Tuple.fromArray(Array.range(0, tup.size)).asInstanceOf[Indices[tup.type]]

  inline def hasDuplicates: HasDuplicates[tup.type] = compiletime.constValue[HasDuplicates[tup.type]]

  inline def mapAs[T](using @unused ev: tup.type containsOnly T)[F[_ <: T]](inline f: [t <: T] => t => F[t])
    : Tuple.Map[tup.type, [X] =>> F[X & T]] =
    tup.map[[X] =>> F[X & T]]([t] => (t: t) => f(t.asInstanceOf[t & T]))

  def toArrayOf[T](using @unused ev: tup.type containsOnly T)(using ClassTag[T]): Array[T] = tup match
    case EmptyTuple => Array.empty[T]
    case self: Product =>
      val arr = new Array[T](self.productArity)
      var i = 0
      while i < arr.length do
        arr(i) = self.productElement(i).asInstanceOf[T]
        i += 1
      arr

type Indices[Tup <: Tuple] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case h *: t => 0 *: IndicesAux[t, 1]

type IndicesAux[Tup <: Tuple, N <: Int] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case h *: t => N *: IndicesAux[t, S[N]]

type HasDuplicates[Tup <: Tuple] <: Boolean = Tup match
  case EmptyTuple => false
  case h *: t => Tuple.Contains[t, h] || HasDuplicates[t]
