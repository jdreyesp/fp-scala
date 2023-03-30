package com.jdreyesp.fpscala.chapter4.errorhandling

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => Right(f(value))
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(value) => Right(value)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => f(value)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap(eith1 => b.map(eith2 => f(eith1, eith2)))
  }
}

object ExercisesEither extends App {

  val e1: Either[Int, Int] = Right(3)
  val e2: Either[Int, Int] = Left(-1)
  val e3: Either[Int, Int] = Left(-3)


  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  es.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight[Either[E, List[A]]](Right(List[A]()))((ei, eithList) => ei.map2(eithList)(_ :: _))
  }

  //  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
  //    traverse(es)(x => x)

  println(e1.map(_ + 1))
  println(e2.map(value => {println("This should not be printed!"); value }))
  println(e1.orElse(throw new Exception("This should never happen!")))
  println(e2.orElse(Right(5)))
  println(e1.flatMap(value => Right(value + 1)))
  println(e1.flatMap(value => Left(value + 1)).map(value => { println("This should never be printed!"); value }))
  println(e1.map2(e2)((a, _) => {println("This should never happen!"); a }))
  println(e1.map2(e1)(_ + _))
  println(sequence(List(e1, e1)))
  println(sequence(List(e1, e2, e1, e3)))
  println(traverse(List(e1, e1))(_ => Right(10)))
  println(traverse(List(e1, e1))(_ => Left(-1)))
}
