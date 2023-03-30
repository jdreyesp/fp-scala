package com.jdreyesp.cats.chapters.chapter1.typeclases

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

import java.time.Instant
import java.util.Date

object CatsShow extends App {

  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show[String]

  println(showInt.show(10))
  println(showString.show("abc"))

  println(123.show)
  println("abc".show)

  implicit val dateShow: Show[Date] = (date) => s"${date.getTime}ms since the epoch."

  println(dateShow.show(Date.from(Instant.now)))
}
