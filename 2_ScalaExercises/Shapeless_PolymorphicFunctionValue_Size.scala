// a function from Int or String or Pair to a size defined by type specific case.

import shapeless.poly._

object size extends Poly1 {
 implicit def caseInt = at[Int](x => 1)
 implicit def caseString = at[String](_.length)
 implicit def caseTuple[T, U](implicit st: Case.Aux[T, Int], su: Case.Aux[U, Int]) =
  at[(T, U)](t => size(t._1) + size(t._2))
}

size(23) // return 1
size("foo") // return 3
size ((23, "foo")) // return 4
size(((23, "foo")), 13) //return 5

object addSize extends Poly2 {
 implicit def default[T](implicit st: size.Case.Aux[T, Int]) =
  at[Int, T] { (acc, t) => acc + size(t)}
}

val example1 = 23 :: "foo" :: (13, "wibble") :: HNil
example1.foldLeft(0)(addSize)
