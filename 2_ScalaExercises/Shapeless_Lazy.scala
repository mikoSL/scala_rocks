/**
* first class lazy value solves implicit recursive problems:
* 1. at value level, recursive type class instances would have to be constructed
* lazily, but Scala does not support lazy implicit arguments.
* 2. at type level, during type checking of expression constructing recursive
* implicit values the implicit resolution mechanism would revisit types in a
* way that would trip the divergence checker.
*/

sealed trait List[+T]
case class Cons[T](hd: T, tl: List[T]) extends List[T]
sealed trait Nil extends List[Nothing]
case object Nil extends Nil

trait Show[T] {
 def apply(t: T): String
}

object Show {
 implicit def showInt: Show[Int] = new Show[Int] {
  def apply(t: Int) = t.toString
 }
 implicit def showNil: Show[Nil] = new Show[Nil] {
  def apply(t: Nil) = "Nil"
 }
 implicit def showCons[T](implicit st: Lazy[Show[T]], sl: Lazy[Show[List[T]]]): Show[Cons[T]] = new Show[Cons[T]] {
  def apply(t: Cons[T]) = s"Cons(${ show(t.hd)(st.value) }, ${ show(t.tl)(sl.value) })"
 }
 implicit def showList[T](implicit sc: Lazy[Show[Cons[T]]]): Show[List[T]] = new Show[List[T]] {
  def apply(t: List[T]) = t match {
   case n: Nil => show(n)
   case c: Cons[T] => show(c)(sc.value)
  }
 }
}

def show[T](t: T)(implicit s: Show[T]) = s(t)

val l: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

show(1) // return string: Cons(1, Cons(2, Cons(3, Nil)))
