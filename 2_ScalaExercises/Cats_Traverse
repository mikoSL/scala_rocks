import scala.concurrent.Future

def parseInt(s: String): Option[Int] = ???

trait SecurityError
trait Credentials

def validateLogin(cred: Credentials): Either[SecurityError, Unit] = ???

trait Profile
trait User

def userInfo(user: User): Future[Profile] = ???

//fetch profile for a List[User]
def profileFor(users: List[User]): List[Future[Profile]] = users.map(userInfo)


// Traverse method. F[_] some sort of context which may contain a value(several)
trait Traverse[F[_]] {
 def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}

//Trasverse walks the F[] depends on the effect of the function.
import cats.Semigroup
import cats.data.{NonEmptyList, OneAnd, Validated, ValidatedNel}
import cats.implicits._

def parseIntEither(s: String): Either[NumberFormatException, Int] =
 Either.catchOnly[NumberFormatException](s.toInt)

def parseIntValidated(s: String): ValidatedNel[NumberFormatException, Int] =
 Validated.catchOnly[NumberFormatException](s.toInt).toValidatedNel


implicits def nelSemigroup[A]: Semigroup[NonEmptyList[A]] =
 OneAnd.oneAndSemigroupK[List].algebra[A]


 
