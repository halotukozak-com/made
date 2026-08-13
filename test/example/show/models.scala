package halotukozak.made.example.show

import halotukozak.made.annotation.transparent

case class User(name: String, age: Int)

@transparent
case class Email(value: String)

sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape
case object Point extends Shape

case object Origin
