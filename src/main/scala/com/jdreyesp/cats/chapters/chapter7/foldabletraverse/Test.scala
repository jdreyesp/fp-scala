import cats.data.Writer

import java.util.Properties
import scala.util.Try


// write an algorithm that takes a list of strings and calculate the average value of the strings,
// convertinng them to ints and discarding the ones that throw an error on parsing
object Test extends App {

//  def average(list: List[String]): Float = {
//    val allowedList = list
//      .map(str => Try(Integer.valueOf(str).intValue()))
//      .filter(_.isSuccess)
//      .map(_.get)
//
//    allowedList.foldLeft(0F)(_+_) / allowedList.size
//  }
//
//  println(average(List("1", "2", "aa", "foo", "3"))) //3+2+1 = 6/3 = 2

  import cats.instances.string._

  val maybeInt: Option[Int] = for {
    _ <- Option(1)
    t <- Try(1 / 0).map(_ + 1).toOption
  } yield t

  val writer: Writer[String, Int] = maybeInt match {
    case None => Writer.apply("Hello", 0)
    case Some(v) => Writer.value(v)
  }

  val writer2 = for {
    a <- writer.tell("Hello")
    b <- writer.map(_ + 10)
  } yield a + b

  println(writer2.run)

  // Write a program that takes an optional[Int], a try[int] and returns the sum of both if they're both Some and Success, or 0 instead
  def exercise1(a: Option[Int], b: Try[Int]): Int = {
    (for {
      a1 <- a.toRight(0)
      a2 <- b.toEither
    } yield a1 + a2) match {
      case Left(_) => 0
      case Right(result) => result
    }
  }

  println(exercise1(Option(1), Try(1)))
  println(exercise1(None, Try(1)))
  // Write a program that takes two futures, and execute them in Parallel. After they're complete,
}



// Write a program that takes configuration from Properties, the first one extracting
// the name of the user, and second extracting the address of the user
// Reequirements: Run these operations in parallel
// Accumulate the result of the errors if they happen (i.e. the configuration is not found)

//Define the config

//val properties: Properties = new Properties()

//def readProperty(key: String): Option[String] = Option(properties.getProperty(key))
//
//object EitherSyntax {
//  implicit class EitherSyn[T](e: Either[Throwable, T]) {
//    def toTuple(): (Vector[String], List[String]) = {
//      e match {
//        case Right(b) => (Vector(""), List(b.toString))
//        case Left(a) => (Vector(a.getMessage), List(""))
//      }
//    }
//  }
//}

//def readProperties(): Either[Vector[String], List[String]] = {
//
//  import EitherSyntax._
//
//  // We represent the errors as a Vector[String] and the result as a List[String]
//  val propertyWrite = Writer[Vector[String], List[String]]
//
//  for {
//    _ <- propertyWrite.tupled(Try(properties.load(new FileReader("config.properties"))).toEither.toTuple)
//    a <- propertyWrite.tupled(Either.cond(readProperty("user.name") == None, readProperty("user.name").get, "Fail").toTuple
//
//  }
//}






