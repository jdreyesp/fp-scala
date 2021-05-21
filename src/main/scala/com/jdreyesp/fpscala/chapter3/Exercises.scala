package com.jdreyesp.fpscala.chapter3

import scala.annotation.tailrec

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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

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

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, b) => b + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) => foldLeft[A, List[A]](tail, List(head))((acc, elem) => append(List(elem), acc))
    }
  }

  def sumFoldLeft(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(head, tail) => foldLeft(tail, head)(_ + _)
  }

  def productFoldLeft(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(head, tail) => foldLeft(tail, head)(_ * _)
  }

  //Steps for a1=[1,2,3] a2=[4,5,6]
  //[1,2,3] foldright 3
  //[1,2,3,4*] foldright 4
  //[1,2,3,4, 5*] foldright 5
  //[1,2,3,4,5,6*]
  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => Nil
    case elems => foldRight[A, List[A]](elems, a2)(Cons(_, _))
  }

  //Steps for a1=[1,2,3] a2=[4,5,6]
  //[3,2,1] //reverse
  //[4,3,2,1] //foldleft
  //[5,4,3,2,1] //foldleft
  //[6,5,4,3,2,1] //foldleft
  //[1,2,3,4,5,6] //reverse
  def appendFoldLeft[A](a1: List[A], a2: List[A]): List[A] = a2 match {
    case Nil => Nil
    case elems => reverse(foldLeft[A, List[A]](elems, reverse(a1))((acc, elem) => Cons(elem, acc)))
  }

  def concatenateLists[A](lists: List[List[A]]): List[A] = {
    lists match {
      case Nil => Nil
      case Cons(list: List[A], restLists: List[List[A]]) => foldLeft[List[A], List[A]](restLists, list)(append(_,_))
    }
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
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h + foldRight(t, 0)(_ + _)
    case _ => 101
  }

  println(x)

  val l = List(1,2,3,4)
  println(tail(Nil))
  println(tail(l))
  println(setHead(Nil, 10))
  println(setHead(List(1), 10))
  println(setHead(l, 10))
  println(drop(l, 1))
  println(dropWhile(l, (elem: Int) => elem < 3))
  println(init(l))
  println(length(l))

  def b(l: List[Int]): Boolean = l match { case Cons(_, _) => true}
  def b2(l: List[Int]): Boolean = l match {case Cons(_, _) => true}

  println(s"B IS: ${b(l) && b2(l)}")

  //scala.collectionList() Constructors are made with foldRights :)
  println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  println(foldLeft(l, 0)(_ + _))
  println(reverse(l))
  println(sumFoldLeft(l))
  println(productFoldLeft(l))
  println(appendFoldRight(l, List(5,6,7)))
  println(appendFoldLeft(l, List(5,6,7)))
  println(concatenateLists(List(l, l, l)))
}
