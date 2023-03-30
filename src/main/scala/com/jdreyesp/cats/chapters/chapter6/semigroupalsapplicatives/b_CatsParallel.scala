package com.jdreyesp.cats.chapters.chapter6.semigroupalsapplicatives

/**
 Definition of Parallel:

 ```scala
 trait Parallel[M[_]] {
  type F[_]
  def applicative: Applicative[F]
  def monad: Monad[M]
  def parallel: ~>[M, F]
 }
 ```

 which means:

 if there is a Parallel instance for some type constructor M then:
• there must be a Monad instance for M;
• there is a related type constructor F that has an Applicative instance;
and
• we can convert M to F.

 */
object CatsParallel extends App {

 import cats.Semigroupal
 import cats.instances.either._
 import cats.syntax.apply._ // for tupled

 // Starting the example with Semigroupal, we see that this computation continues to be sequential
 // (due to calls to flatMap + map in Monads).
 type ErrorOr[A] = Either[Vector[String], A]

 val error1: ErrorOr[Int] = Left(Vector("Error 1"))
 val error2: ErrorOr[Int] = Left(Vector("Error 2"))

 println(Semigroupal[ErrorOr].product(error1, error2))
 //another syntax:
 println((error1, error2).tupled)

 import cats.syntax.parallel._ // for parTupled
 import cats.implicits.catsKernelStdMonoidForVector

 // With Parallel, we can see both error results, since it's executed in parallel and aggregated at the end
 println((error1, error2).parTupled)

 // Using `parMapN` utility method:
 val success1: ErrorOr[Int] = Right(1)
 val success2: ErrorOr[Int] = Right(2)
 val addTwo = (x: Int, y: Int) => x + y

 println((error1, error2).parMapN(addTwo))
 println((success1, success2).parMapN(addTwo))

}
