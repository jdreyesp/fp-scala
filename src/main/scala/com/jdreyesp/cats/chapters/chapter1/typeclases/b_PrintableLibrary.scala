package com.jdreyesp.cats.chapters.chapter1.typeclases

import scala.util.Try


trait Printable[A] {
  def format(a: A): String
}

object PrintableInstances {
  implicit val printableString: Printable[String] = a => a
  implicit val printableInt: Printable[Int] = a => Try(String.valueOf(a)).getOrElse("")
}

// Using method extension
object PrintableSyntax {
  implicit class PrintableSyntax[A](a: A) {
    def asString(implicit printable: Printable[A]): String = printable.format(a)
  }
}

object Printable {
  def format[A](a: A)(implicit printable: Printable[A]): String = {
    printable.format(a)
  }

  def print[A](a: A)(implicit printable: Printable[A]): Unit = {
    println(printable.format(a))
  }
}

object PrintableApp extends App {
  import PrintableInstances._
  import PrintableSyntax._

  val s: String  = "This is a string"
  val i: Int = 10

  println(Printable.format(s))
  println(Printable.format(i))
  // println(Printable.format(1234D)) //this should show a compiler error
  Printable.print(s)
  Printable.print(i)

//  println(s.asString)
//  println(i.asString)
}
