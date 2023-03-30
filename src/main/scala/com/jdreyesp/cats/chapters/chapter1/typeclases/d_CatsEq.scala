package com.jdreyesp.cats.chapters.chapter1.typeclases

import cats.instances.int._
import cats.kernel.Eq

object CatsEqApp extends App {

  val eqInt = Eq[Int]

  println(eqInt.eqv(123, 123))
}
