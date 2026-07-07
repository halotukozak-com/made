package made
package annotation

import scala.quoted.*

/**
 * Provides an explicit default value for a constructor parameter with
 * highest priority in the default resolution chain.
 *
 * The value is by-name (`=> T`) so it is evaluated lazily each time
 * `default` is called on the corresponding [[made.MadeFieldElem]].
 *
 * Priority chain: `@whenAbsent` > `@optionalParam` > constructor default.
 *
 * @tparam T the type of the default value
 * @param v the default value, evaluated lazily
 * @see [[made.MadeFieldElem]]
 * @see [[optionalParam]]
 */
class whenAbsent[+T](v: => T) extends MetaAnnotation:
  def value: T = v

object whenAbsent:
  inline def value[T]: T = ${ valueImpl[T] }

  // $COVERAGE-OFF$
  private def valueImpl[T: Type](using quotes: Quotes): Expr[T] =
    import quotes.reflect.*

    object DefaultValueMethod:
      def unapply(s: Symbol): Option[Symbol] = s match
        case ms if ms.isDefDef =>
          ms.name match
            case DefaultParamAccessorName(actualMethodName: String, idx: String) =>
              val method = actualMethodName match
                case "$lessinit$greater" =>
                  ms.owner.companionModule.companionClass.primaryConstructor
                case name =>
                  ms.owner.methodMember(name).headOption getOrElse report.errorAndAbort(
                    s"whenAbsent.value macro could not find method '$name' in ${ms.owner.fullName}",
                  )

              method.paramSymss.flatten.lift(idx.toInt - 1)
            case _ => None
        case _ => None

    val owner = Symbol.spliceOwner.owner match
      case DefaultValueMethod(paramSymbol) => paramSymbol
      case other => other

    owner.getAnnotation(TypeRepr.of[whenAbsent[T]].typeSymbol) match
      case Some(annot) => '{ ${ annot.asExprOf[whenAbsent[T]] }.value }
      case _ =>
        report.error("whenAbsent.value can only be used inside a parameter annotated with @whenAbsent")
        '{ ??? }
// $COVERAGE-ON$
