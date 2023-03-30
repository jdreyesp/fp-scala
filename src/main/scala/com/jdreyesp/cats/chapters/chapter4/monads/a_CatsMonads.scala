package com.jdreyesp.cats.chapters.chapter4.monads

import scala.util.Try

// Monad definition:
/*
A monad is a mechanism for sequencing computations. It needs to have a pure and a flatMap function (that we can
introduce in a for comprehension).

trait Monad[F[_]] {
  def pure[A](value: A): F[A]
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
}

Because three equational laws have to apply:

Left identity: calling pure and transforming the result with func is the
same as calling func:
pure(a).flatMap(func) == func(a)


Right identity: passing pure to flatMap is the same as doing nothing:
m.flatMap(pure) == m


Associativity: flatMapping over two functions f and g is the same as
flatMapping over f and then flatMapping over g:
m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))



Note: A monad ensures sequential computation. If you want for instance to parallelize the computation of two Futures,
that's a different discussion, since in a for loop the computation is sequential.

Note: Remember, a monad is ALSO a functor (it also implements map)
 */
object CatsMonads extends App {

  // Demonstration of Option as monad
  def parseInt(str: String): Option[Int] = Try(Integer.valueOf(str).intValue()).toOption
  def divide(i: Int, i1: Int): Option[Int] = Try(i / i1).toOption

  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    parseInt(aStr).flatMap(str1 => parseInt(bStr).flatMap(str2 => divide(str1, str2)))

  // Equivalent with for comprehension
  def stringDivideBy2(aStr: String, bStr: String): Option[Int] =
  for {
      aNum <- parseInt(aStr)
      bNum <- parseInt(bStr)
      ans <- divide(aNum, bNum)
    } yield ans


  println(stringDivideBy("10", "2"))
  println(stringDivideBy2("10", "2"))


  // Using cats
  import cats.Monad
  import cats.instances.option._

  val opt1 = Monad[Option].pure(3)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a * 3))
  val opt3 = Monad[Option].map(opt2)(_ * 100)

  println(opt1)
  println(opt2)
  println(opt3)

  // Another example

  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatmap
  import cats.instances.list._
  import cats.Id


  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
    a.flatMap(i1 => b.map(i2 => i1*i1 + i2*i2)) //Same as for comprehension for { i1 <- a ; i2 <- b ; yield (i1*i1 + i2*i2)
  }

  println(sumSquare(Option(2), Option(3)))
  println(sumSquare(List(1,2,3), List(4,5)))
  println(sumSquare(Monad[Id].pure(1), Monad[Id].pure(2)))
}
