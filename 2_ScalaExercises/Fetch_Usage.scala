// implement Datasource type

import cats.data.NonEmptyList
import cats.instances.list._
import fetch._
import fetch.syntax._
import fetch.unsafe.implicits._


trait Datasource[Identity, Result] {
 def name: String
 def fetchOne(id: Identity): Query[Option[Result]]
 def fetchMany(ids: NonEmptyList[Identity]): Query[Map[Identity, Result]]
}

implicit object ToStringSource extends Datasource[Int, String] {
 override def name = "ToString"

 override def fetchOne(id: Int): Query[Option[String]] = {
  Query.sync({
   println(s"[${Thread.currentThread.getId}] One ToString $id")
   Option(id.toString)
  })
 }

 override def fetchMany(ids: NonEmptyList[Int]): Query[Map[Int, String]] = {
  Query.sync({
   println(s"[${Thread.currentThread.getId}] Many ToString $ids")
   ids.toList.map(i => (i, i.toString)).toMap
  })
 }
}

import ToStringSource._
def fetchString(n: Int): Fetch[String] = Fetch(ToStringSource)

//fetch users by given ids
type UserId = Int
case class User(id: UserId, username: String)

//simulate unpredictable latency
def latency[A](result: A, msg: String) = {
 val id = Thread.currentThread.getId
 println(s"~~> [$id] $msg")
 Thread.sleep(100)
 println(s"<~~[$id] $msg")
 result
}

//emulate a database with an in-memory map
val userDatabase: Map[UserId, User] = Map(
 1 -> User(1, "@one"),
 2 -> User(2, "@two"),
 3 -> User(3, "@three"),
 4 -> User(4, "@four")
)

implicit object UserSource extends Datasource[UserId, User] {
 override def name = "User"
 override def fetchOne(id: UserId): Query[Option[User]] = {
  Query.sync({
   latency(userDatabase.get(id), s"One User $id")
  })
}
 override def fetchMany(ids: NonEmptyList[UserId]): Query[Map[UserId, User]] = {
  Query.sync({
   latency(userDatabase.filterKeys(ids.toList.contains), s"Many Users $ids")
  })
 }
}

//fetch users by given ids

def getUser(id: UserId): Fetch[User] = Fetch(id)(UserSource)
