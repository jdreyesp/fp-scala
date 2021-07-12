package com.jdreyesp.fpscala.chapter3.datastructures

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

  def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, List())((acc, elem) => append(List(elem), acc))

  def sumFoldLeft(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(head, tail) => foldLeft(tail, head)(_ + _)
  }

  def productFoldLeft(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(head, tail) => foldLeft(tail, head)(_ * _)
  }

  //A=Int l=[1,2,3]   [1] => [2,1] => [3,2,1]
  //A=Int l=[1,2,3]   [1] => [1,2] => [1,2,3]
  def identity[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, List())((b, a) => Cons(a, b))

  //foldLeft(l, 0)(_ + _) =>


  //Steps for a1=[1,2,3] a2=[4,5,6]
  //[1,2,3] foldright 3
  //[1,2,3,4*] foldright 4
  //[1,2,3,4, 5*] foldright 5
  //[1,2,3,4,5,6*]
  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  //Steps for a1=[1,2,3] a2=[4,5,6]
  //[4,5,6] //reverse O(2n) = O(n)
  //[3,4,5,6] //foldleft
  //[2,3,4,5,6] //foldleft
  //[1,2,3,4,5,6] //foldleft
  def appendFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((ax, a) => Cons(a, ax))

  def concatenateLists[A](lists: List[List[A]]): List[A] = foldLeft[List[A], List[A]](lists, List())(append(_,_))

  def addOne(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case Cons(head, tail) => reverse(foldLeft[Int, List[Int]](tail, List(head + 1))((acc, elem) => Cons(elem+1, acc)))
    }
  }

  def doubleToString(list: List[Double]): String = {
    list match {
      case Nil => ""
      case Cons(head, tail) => foldLeft(tail, head.toString)(_ + _)
    }
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = reverse(foldLeft(as, List[B]())((a, az) => Cons(f(az), a)))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, List[B]())((az, a) => append(f(az), a))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List[A]())((az, a) => if(f(az)) Cons(az, a) else a)

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(i => if(f(i)) List(i) else Nil)

  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] = (a1, a2) match {
    case (Nil, Nil) => Nil
    case (a, Nil) => a
    case (Nil, b) => b
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
//
//  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
//
//  }

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
  //scala.collectionList() Constructors are made with foldRights :)
  println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  println(foldLeft(l, 0)(_ + _))
  println(reverse(l))
  println(sumFoldLeft(l))
  println(productFoldLeft(l))
  println(appendFoldRight(l, List(5,6,7)))
  println(appendFoldLeft(l, List(5,6,7)))
  println(concatenateLists(List(l, l, l)))
  println(addOne(l))
  println(doubleToString(List(1D, 2D, 3D)))
  println(map(l) {_+1})
  println(filter(l)(_ < 4))
  println(flatMap(l)(i => List(i,i)))
  println(filterWithFlatMap(l)(_ < 4))
  println(zipWith(l, l)(_ + _))
}
