package made

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.quoted.*

transparent inline def mapTuple(tup: Tuple)[U](inline f: [e <: U] => e => Any)(using tup.type containsOnly U): Tuple =
  ${ mapTupleImpl[U, tup.type]('tup, 'f) }

def mapTupleImpl[U: Type, Tup <: Tuple : Type](tup: Expr[Tup], f: Expr[[e <: U] => e => Any])(using quotes: Quotes): Expr[Tuple] =
  import quotes.reflect.*

  f.asTerm.underlying match
    case Block(
          List(DefDef(_, List(TypeParamClause(List(tparam)), TermParamClause(List(ValDef(param, _, _)))), _, Some(rhs))),
          _,
        ) =>
      val tparamSymbol = tparam.symbol

      def callRhs(index: Int, elemType: Type[?]): Expr[?] = {
        new TreeMap:
          override def transformTerm(tree: Term)(owner: Symbol): Term =
            if tree.symbol.name == param then '{ $tup(${ Expr(index) }) }.asTerm else {
              val term = tree match
                case block: Block => block.changeOwner(owner)
                case other => other
              super.transformTerm(term)(owner)
            }

          override def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree =
            val elemTpe = elemType match
              case '[t] => TypeRepr.of[t]
            val substituted = tree.tpe.substituteTypes(List(tparamSymbol), List(elemTpe))
            if substituted != tree.tpe then
              substituted.asType match
                case '[t] => TypeTree.of[t]
            else super.transformTypeTree(tree)(owner)
      }.transformTerm(rhs)(Symbol.spliceOwner).asExpr

      @tailrec def loop[tuple <: Tuple : Type](acc: Vector[Expr[Any]], index: Int): Expr[Tuple] = Type.of[tuple] match
          case '[EmptyTuple]  =>
            Expr.ofRefinedTuple(acc.toList)
          case '[h *: tail]  =>
            val headExpr = callRhs(index, Type.of[h])
            loop[tail](acc :+ headExpr, index + 1)

      loop[Tup](Vector.empty, 0)

transparent inline def mapTuple[Tup <: Tuple, U](inline f: [e <: U] => e => Any)(using Tup containsOnly U): Tuple =
  ${ mapTupleImpl[U, Tup]('f) }

def mapTupleImpl[U: Type, Tup <: Tuple : Type](f: Expr[[e <: U] => e => Any])(using quotes: Quotes): Expr[Tuple] =
  import quotes.reflect.*

  f.asTerm.underlying match
    case Block(List(DefDef(_, List(_, TermParamClause(List(ValDef(param, _, _)))), _, Some(rhs))), _) =>
      def callRhs(index: Int): Expr[?] = {
        new TreeMap:
          override def transformTerm(tree: Term)(owner: Symbol): Term =
            if tree.symbol.name == param then '{ compiletime.erasedValue[Tup](${ Expr(index) }) }.asTerm else {
              val term = tree match
                case block: Block => block.changeOwner(owner)
                case other => other
              super.transformTerm(term)(owner)
            }
      }.transformTerm(rhs)(Symbol.spliceOwner).asExpr

      @tailrec def loop[tuple <: Tuple : Type](acc: Vector[Expr[Any]], index: Int): Expr[Tuple] = Type.of[tuple] match
          case '[EmptyTuple]  =>
            Expr.ofRefinedTuple(acc.toList)
          case '[h *: tail]  =>
            val headExpr = callRhs(index)
            loop[tail](acc :+ headExpr, index + 1)

      loop[Tup](Vector.empty, 0)
