package halotukozak
package made

sealed trait NotExists

case object NotExists extends NotExists

extension (any: AnyRef | NotExists)
  transparent inline def exists: Boolean = inline any match
    case NotExists => false
    case _ => true

  transparent inline def notExists: Boolean = !any.exists
