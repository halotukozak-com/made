package halotukozak
package made

/** Marks an absent value, e.g. a field without a default or a type without a companion. */
case object NotExists

/** The singleton type of [[NotExists]], so `inline match` on `case NotExists` reduces statically. */
type NotExists = NotExists.type
