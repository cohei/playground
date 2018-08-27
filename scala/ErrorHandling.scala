import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import scala.collection.immutable.Map

object ErrorHandling {
  def main(args: Array[String]): Unit = {
    exampleFuture()
    exampleOtion()
    exampleTry()
  }

  def exampleFuture(): Unit =
    Future(fail("1") + fail("2")).onComplete {
      case Failure(e) => println(s"NG: ${e.getMessage}")
      case Success(_) => println("OK")
    }

  def fail(cause: String): Int = throw new RuntimeException(cause)

  def exampleOtion(): Unit = {
    val myScore: Map[String, Int] = Map("English" -> 30, "Math" -> 80)

    println(myScore.get("English"))
    println(myScore.get("Japanese"))
  }

  def exampleTry(): Unit = {
    println(Try[Int](4 / 2))
    println(Try[Int](4 / 0))
  }

  def useFindValidUser(): Unit =
    findValidUser match {
      case Left(NotFound(id)) => println(s"User(${id}) is absent.")
      case Right(user) => println(s"Found: ${user.name}")
  }

  def findValidUser: Either[MyError, User] = ???

  sealed trait MyError

  case class NotFound(id: Long) extends MyError

  case class Expired(user: User) extends MyError

  case class User(name: String)
}
