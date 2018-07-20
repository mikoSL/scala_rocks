//Use Either for exception.
 object EitherStyle {
  def parse(s: String): Either[NumberFormatException, Int] =
   if (s.matches("-?[0-9]+")) Either.right(s.toInt)
   else Either.left(new NumberFormatException(s"${s} is not a valid integer."))

  def reciprocal(i: Int): Either[IllegalArgumentException, Double] =
   if (i == 0) Either.left(new IllegalArgumentException("Cannot take reciprocal of 0."))
   else Either.right(1.0/i)

  def stringify(d: Double): String = d.toString

  def magic(s: String): Either[Exception, String] =
   parse(s).flatMap(reciprocal).map(stringify)
 }

 //use pattern match on each case the Either returned by magic can be in.
 import EitherStyle._

 val result = magic("2") match {
  case Left(_: NumberFormatException) => "Not a number!"
  case Left(_: IllegalArgumentException) => " Can not take reciprocal of 0!"
  case Left(_) => "Unknown error"
  case Right(result) => s"Got reciprocal: ${result}"
 }

/**
* instead of using exceptions as our error value,
* enumerate explicitly the thing that can go wrong in program.
*/
object EitherStyleWithAdts {
 sealed abstract class Error
 final case class NotANumber(String: String) extends Error
 final case object NoZeroReciprocal extends Error

 def parse(s: String): Either[Error, Int] =
  if (s.matches("-?[0-9]+")) Either.right(s.toInt)
  else Either.left(NotANumber(s))

 def reciprocal(i: Int): Either[Error, Double] =
  if (i == 0) Either.left(NoZeroReciprocal)
  else Either.right(1.0 / i)

 def stringify(d: Double): String = d.toString

 def magic(s: String): Either[Error, String] =
  parse(s).flatMap(reciprocal).map(stringify)
}

import EitherStyleWithAdts._

val result = magic("n") match {
 case Left(NotANumber(_)) => "Not a number"
 case Left(NoZeroReciprocal) => " Can not take reciprocal of 0!"
 case Right(result) => s"Got reciprocal: ${result}"
}

// interact with exception-throwing error
val either: Either[NumberFormatException, Int] =
 try {
  Either.right("abc".toInt)
 } catch {
  case nfe: NumberFormatException => Either.left(nfe)
 }

 //equivalent with above code with less code
 val either: Either[NumberFormatException, Int] =
  Either.catchOnly[NumberFormatException]("abc".toInt)

//application-wide error ADT what wraps each error ADT
sealed abstract class DatabaseError
trait DatabaseValue

object Database {
 def databaseThings(): Either[DatabaseError, DatabaseValue] = ???
}

sealed abstract class ServiceError
trait ServiceValue

object Service {
 def serviceThings(): Either[ServiceError, ServiceValue] = ???
}

sealed abstract class AppError
object AppError {
 final case class Database(error: DatabaseError) extends AppError
 final case class Service(error: ServiceError) extends AppError
}

def doApp: Either[AppError, ServiceValue] =
 Database.databaseThings().leftMap(AppError.Database).
  flatMap(dv => Service.serviceThings(dv).leftMap(AppError.Service))

/**
* then it can take action on entire classes of errors
* instead of having to pattern match on each individual one.
*/
def awesome =
 doApp match {
  case Left(AppError.Database(_)) => "Sth wrong in database"
  case Left(AppError.Service(_)) => "sth wrong in service"
  case Right(_) => "everything is right!"
 }
