abstract class Term
case class Var(name: String) extends Term
case class Fun(arg: String, body: Term) extends Term
case class App(f: Term, v: Term) extends Term

object TermTest extends Application {
 def printTerm(term: Term) {
  term match {
   case Var(n) =>
    print(n)
   case Fun(x, b) =>
    print("^" + x + ".")
    printTerm(b)
   case App(f, v) =>
    print("(")
    printTerm(f)
    print(" ")
    printTerm(v)
    print(")")
  }
 }
 def isIdentifyFun(term: Term): Boolean = term match {
  case Fun(x, Var(y)) if x == y => true
  case _ => false
 }
 val id = Fun("x", Var("x"))
 val t = Fun("x", Fun("y", App(Var("x"), Var("y"))))
 printTerm(t)
 println
 println(isIdentifyFun(id))
 println(isIdentifyFun(t))
}

//Monad OptionT: compose outer monad (F[]) with a specific inner monad (option)

case class OptionT[F[_], A](value: F[Option[A]])

implicit def optionTMonad[F[_]](implicit F: Monad[F]) = {
 new Monad[OptionT[F, ?]] {
  def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))
  def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
   OptionT {
    F.flatMap(fa.value) {
     case None => F.pure(None)
     case Some(a) => f(a).value
    }
   }
  def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] “=
   defaultTailRecM(a)(f)”
 }
}
