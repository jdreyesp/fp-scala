package com.jdreyesp.cats.chapters.chapter1.typeclases

final case class Person(name: String, email: String)

sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
final case object JsNull extends Json

// The "serialize to JSON" behaviour is encoded in this trait
trait JsonWriter[A] {
  def write(value: A): Json
}


// Using type class instances
object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] =
    (value: String) => JsString(value)
  implicit val personWriter: JsonWriter[Person] =
    (value: Person) => JsObject(Map(
      "name" -> JsString(value.name),
      "email" -> JsString(value.email)
    ))

  // What about Option[A]? we don't want to bruteforece Option[Person], Option[String], so we delegate to the underneath
  // JsonWriter by:
  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    (option: Option[A]) => option match {
      case Some(aValue) => writer.write(aValue) //delegating!
      case None => JsNull
    }

  // etc...
}

// Using extension methods
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = {
      w.write(value)
    }
  }
}

object TypeClasses extends App {

  import JsonSyntax._
  import JsonWriterInstances._

  def toJson[A](a: A)(implicit jsonWriter: JsonWriter[A]): Json = {
    jsonWriter.write(a)
  }

  val person = Person("Diego", "jdrp1984@gmail.com")
  println(toJson(person))
  println(person.toJson)
  println(toJson(Option("Hello"))) // Using the recursive implicit resolution from JsonWriter[Option[A]] to JsonWriter[A]

  //With implicitly
  val jsonWriter: JsonWriter[Person] = implicitly[JsonWriter[Person]]
  println(toJson(person)(jsonWriter))
}


