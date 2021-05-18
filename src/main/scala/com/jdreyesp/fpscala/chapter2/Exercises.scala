package com.jdreyesp.fpscala.chapter2

object Exercises extends App {

  //Functions as literals

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.isEmpty || as.size == 1) true
    else if (as.size == 2) ordered(as.head, as.tail.head)
    else ordered(as.head, as.tail.head) && isSorted(as.tail, ordered)
  }

  println(isSorted(Array("Hello", "World", "Zip", "Alpha", "Beta", "Gamma"), (s1: String, s2: String) => s1.compareTo(s2) < 0))
  println(isSorted(Array("Hello", "World", "Zip"), (s1: String, s2: String) => s1.compareTo(s2) < 0))
  println(isSorted(Array("Hello"), (s1: String, s2: String) => s1.compareTo(s2) < 0))
  println(isSorted(Array(), (s1: String, s2: String) => s1.compareTo(s2) < 0))

  //High order functions
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  println(curry[Int, String, Double]((e1: Int, e2: String) => e1.toDouble + e2.toDouble)(3)("5"))

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  println(compose[String, String, String]((s: String) => s + " World", (s: String) => s + " Hello")("John"))
}
