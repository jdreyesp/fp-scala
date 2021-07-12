package com.jdreyesp.fpscala.chapter5.lazyness

import Stream.{cons, constant, empty, fibs, from}


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

  def exists(f: A => Boolean): Boolean = {
    foldRight(false)((a, b) => f(a) || b)
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
      case Cons(_, _) => empty
    }
  }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((elem, strm) => if(p(elem)) cons(elem, strm) else empty)


  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Empty => true
      case Cons(h, _) if !p(h()) => false
      case Cons(_, t) => t().forAll(p)
    }
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((elem, strm) => cons(f(elem), strm))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((elem, strm) => if(f(elem)) strm else cons(elem, strm))
  }

  def append[B>:A](s: Stream[B]): Stream[B] = {
    foldRight(s)((elem, strm) => cons(elem, strm))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((elem, strm) => f(elem).append(strm))
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
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

  def constant[A](a: A): Stream[A] = {
    lazy val const: Stream[A] = Cons(() => a, () => const)
    const
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def fibs(): Stream[Int] = {
    def fibonacci(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, fibonacci(n2, n1 + n2))
    }
    fibonacci(0, 1)
  }

}

object ExercisesStream extends App {

  val s: Stream[Int] = Stream(1,2,3,4,5)

  println(s.toList())
  println(s.take(3).toList())
  println(s.drop(3).toList())
  println(s.takeWhile(_<3).toList())
  println(s.takeWhileFoldRight(_<3).toList())
  println(s.forAll(_<3))
  println(s.map((a: Int) => a.toDouble).toList())
  println(s.flatMap((a: Int) => Stream.apply(a.toDouble)).toList())
  println(s.filter(_==3).toList())
  println(s.append(Stream(6,7,8)).toList())

  val ones: Stream[Int] = Stream.cons(1, ones)

  println(ones.take(5).toList())
  println(ones.exists(_ % 2 != 0))

  println(ones.map(_ + 1).exists(_ % 2 == 0))
  println(ones.takeWhile(_ == 1))
  println(ones.forAll(_ != 1))

  println(constant(5).take(5).toList())
  println(from(1).take(5).toList())
  println(fibs().take(5).toList())
}
