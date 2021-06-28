package com.jdreyesp.fpscala.chapter5

import com.jdreyesp.fpscala.chapter5.Stream.{cons, empty}


sealed trait Stream[+A] {
  def toList(): List[A] = {
    def toListAcc(s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Empty => acc
        case Cons(h , t) => toListAcc(t(), acc :+ h()) //Why in the book he uses `h() :: acc` and then reverse??
      }
    }
    toListAcc(this, List())
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case Cons(h, t) if n == 0 => cons(h(), t())
      case _ => empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case Cons(h, _) => empty
    }
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  //We make them lazy so that we compute them only once and we cache it for repeated computations
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

}

object ExercisesStream extends App {

  val s: Stream[Int] = Stream(1,2,3,4,5)

  println(s.toList())
  println(s.take(3).toList())
  println(s.drop(3).toList())
  println(s.takeWhile(_<3).toList())
}
