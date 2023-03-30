package com.jdreyesp.cats.chapters.chapter4.monads

/**
 * cats.data.Reader is a monad that allows us to sequence operations that de‐
pend on some input. Instances of Reader wrap up functions of one argument,
providing us with useful methods for composing them.
One common use for Readers is dependency injection. If we have a number
of operations that all depend on some external configuration, we can chain
them together using a Reader to produce one large operation that accepts
the configuration as a parameter and runs our program in the order specified.
 */
object CatsReaderMonad extends App {

  import cats.data.Reader

  // Cats greet and feed example
  final case class Cat(name: String, favouriteFood: String)

  val catNameReader: Reader[Cat, String] = Reader(cat => cat.name)
  val greetingReader: Reader[Cat, String] = catNameReader.map(name => s"Hello $name")
  val feederReader: Reader[Cat, String] = Reader(cat => s"Here's a bowl of ${cat.favouriteFood}")

  val greetAndFeedReader: Reader[Cat, String] = for {
    greeting <- greetingReader
    feed <- feederReader
  } yield s"$greeting. $feed"

  println(greetAndFeedReader.run(Cat("Garfield", "Lasagne")))

  // Exercise
  final case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] = Reader(db => db.passwords.get(username).map(_.equalsIgnoreCase(password)).getOrElse(false))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      userName <- findUsername(userId)
      password <- checkPassword(userName.getOrElse(""), password)
    } yield password
  }

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))
}
