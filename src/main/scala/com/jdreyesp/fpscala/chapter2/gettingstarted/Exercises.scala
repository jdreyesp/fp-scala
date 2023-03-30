package com.jdreyesp.fpscala.chapter2.gettingstarted

import scala.annotation.tailrec

object Exercises extends App {


  def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.isEmpty || as.size == 1) true
    else as match {
      case Array(a, b) => ordered(a,b)
      case ar @ Array(a, b, _*) => ordered(a, b) && isSorted2(ar.tail, ordered)
    }
  }


  //Functions as literals
  @tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.isEmpty || as.size == 1) true
    else if (as.size == 2) ordered(as.head, as.tail.head)
    else ordered(as.head, as.tail.head) && isSorted(as.tail, ordered)
  }

  println(isSorted2(Array("Hello", "World", "Zip", "Alpha", "Beta", "Gamma"), (s1: String, s2: String) => s1.compareTo(s2) < 0))
  println(isSorted2(Array("Alpha", "Beta", "Gamma", "Hello", "World", "Zip"), (s1: String, s2: String) => s1.compareTo(s2) < 0))
  println(isSorted2(Array("Hello", "World", "Zip"), (s1: String, s2: String) => s1.compareTo(s2) < 0))
  println(isSorted2(Array("Hello"), (s1: String, s2: String) => s1.compareTo(s2) < 0))
  println(isSorted2(Array(), (s1: String, s2: String) => s1.compareTo(s2) < 0))

  //High order functions
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  println(curry((e1: Int, e2: String) => e1.toDouble + e2.toDouble)(3)("5"))

  def myfunction(a: Int)(b: Int)(c: Int): Int = {
    a + b + c
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  val mf1: Int => Int => Int = myfunction(1)_
  val mf2: Int => Int = mf1(2)
  val myval: Int = mf2(3)

  val myFunction2 = new (Int => Int) {
    override def apply(v1: Int): Int = v1
  }

  compose(mf1, myFunction2)

  println(myval)
  println(compose[String, String, String]((s: String) => s + " World", (s: String) => s + " Hello")("John"))
}
