package com.jdreyesp.cats.chapters.chapter4.monads

/** cats.data.Writer is a monad that lets us carry a log along with a compu‐
tation. We can use it to record messages, errors, or additional data about a
computation, and extract the log alongside the final result.
One common use for Writers is recording sequences of steps in multi‐
threaded computations where standard imperative logging techniques can re‐
sult in interleaved messages from different contexts. With Writer the log for
the computation is tied to the result, so we can run concurrent computations
without mixing logs */
object CatsWriterMonad extends App {

  import cats.data.Writer

  //First part of the writer are the logs (that could be the output logs from a sequence of computations).
  // Second part is the value that has a meaning alongside the logs
  val writer = Writer[Vector[String], Int](Vector("It was the best of times", "It was the worst of times"), 1859)

  //In case we don't care about logs and want to represent a computational result, we just use Monoid and Applicative
  // Monoid for representing
  type Logged[A] = Writer[Vector[String], A]

  import cats.instances.vector._ // for Monoid
  import cats.syntax.applicative._ // for pure

  // Applicative
  val logged: Logged[Int] = 123.pure[Logged]

  println(logged)

  // In case we only have the message logs and no result, we could use Writer[Unit]
  import cats.syntax.writer._ //for tell

  val logs: Vector[String] = Vector("msg1", "msg2", "msg3")
  println(logs.tell)

  // Examples

  val writer1 =
    for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  println(writer1)

  //Map the value
  println(writer1.map(_ + 10))

  //Map the log
  println(writer1.mapWritten(_.map(_.toUpperCase())))

  //Both (.bimap or .mapBoth)
  println(writer1.bimap(_.map(_.toUpperCase()), _ + 10))


  //Exercise (factorial)
  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  // Using pure Semigroup instance for Vector (by importing cats.instances.vector._)
  // Using tell extension method for Vector (by importing cats.syntax.writer._)
  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- slowly(if (n == 0) 1.pure[Logged] else factorial(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._

  println(Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5))
  )), 5.seconds))
}
