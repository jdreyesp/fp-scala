package com.jdreyesp.fpscala.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Exercises extends App {

  import List._

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  println(x)

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }
  }

  def setHead[A](l: List[A], newValue: A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, tail) => Cons(newValue, tail)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Nil => Nil
      case elem @ Cons(head, tail) => if(n > 0) drop(tail, n - 1) else elem
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case elem @ Cons(head, tail) => if(f(head)) dropWhile(tail, f) else elem
    }
  }

  //Avg: O(log(n). Worst case O(n)
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  //O(n * log(n))
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, Cons(_, Nil)) => Cons(head, Nil)
      case Cons(head, tail) => append(Cons(head, Nil), init(tail))
    }
  }

  val l = List(1,2,3,4)
  println(tail(Nil))
  println(tail(l))
  println(setHead(Nil, 10))
  println(setHead(List(1), 10))
  println(setHead(l, 10))
  println(drop(l, 1))
  println(dropWhile(l, (elem: Int) => elem < 3))
  println(init(l))
}
