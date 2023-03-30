package com.jdreyesp.cats.chapters.chapter4.monads

import CatsMonads.divide

import scala.util.Try

// Either monad
// extension methods like .asRight allows us to return Either instead of Left or Right, making the Either "right-biased",
// and therefore being composable as if it was a Monad.
object CatsEitherMonad extends App {

  import cats.syntax.either._

  /**
   * Counts number of positives in a list
   *
   * @param nums
   * @return number of positive numbers or an error String if a negative is found
   */
  def countPositive(nums: List[Int]): Either[String, Int] =
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative. Stopping!")
      }
    }

  println(countPositive(List(1, 2, 3)))
  println(countPositive(List(1, -2, 3)))

  //Other utility methods from cats Either
  println(Either.catchOnly[NumberFormatException]("foo".toInt))
  println(Either.fromTry(Try("foo".toInt)))
  println(Either.fromOption(None, "badness"))
  println(Either.asLeft[Int].getOrElse(0))
  println(Either.asLeft[Int].orElse(1.asRight))
  println((-1).asRight[String].ensure("Must be non-negative!")(_ > 0))
  println("error".asLeft[Int].recover {
    case _: String => -1
  })
  println("error".asRight.swap)

  println(for {
    a <- 1.asRight
    b <- 0.asRight
    c <- Either.fromOption(divide(a, b), "DIV0")
  } yield c * 100)

  // For error handling
  object wrapper {
    sealed trait LoginError extends Product with Serializable

    final case class UserNotFound(username: String)
      extends LoginError

    final case class PasswordIncorrect(username: String)
      extends LoginError

    case object UnexpectedError extends LoginError
  };

  import wrapper._

  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  // Choose error-handling behaviour based on type:
  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u) =>
        println(s"User not found: $u")
      case PasswordIncorrect(u) =>
        println(s"Password incorrect: $u")
      case UnexpectedError =>
        println(s"Unexpected error")
    }

  val result1: LoginResult = User("dave", "passw0rd").asRight
  println(result1)
  val result2: LoginResult = UserNotFound("dave").asLeft
  handleError(result2.swap.getOrElse(UnexpectedError))

}
