package com.jdreyesp.cats.chapters.chapter6.semigroupalsapplicatives

/**
 * When we validate a form we want to
*return all the errors to the user, not stop on the first error we encounter. If we
*model this with a monad like Either, we fail fast and lose errors.
 *
 *Another example is the concurrent evaluation of Futures. If we have several
*long‐running independent tasks, it makes sense to execute them concurrently.
*However, monadic comprehension only allows us to run them in sequence.
*map and flatMap aren’t quite capable of capturing what we want because
*they make the assumption that each computation is dependent on the previous
*one
 * We'll see that Semigroupal is the baseline for joining contexts, that will be used by Parallel to achieve
 * what we just described. So why bother with Semigroupal at all? The answer is that we can create
useful data types that have instances of Semigroupal (and Applicative)
but not Monad. This frees us to implement product in different ways.
 *
 *Note: While Semigroup allows us to join values, Semigroupal allows us to join con‐
*texts.
 *
 *
 *There is only one law for Semigroupal: the product method must be asso‐
*ciative.
 *product(a, product(b, c)) == product(product(a, b), c)
 *
 */
object CatsSemigroupal extends App {

 import cats.Semigroupal
 import cats.instances.option._

 println(Semigroupal[Option].product(Some(123), Some("abc"))) //Some((123,"abc"))
 println(Semigroupal[Option].product(None, Some("abc"))) //None
 println(Semigroupal.tuple3(Option(1), Option(2), Option(3))) //Some((1, 2, 3))
 println(Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)) //Some(6)

 import cats.syntax.apply._ // for tupled and mapN

 println((Option(123), Option("abc")).tupled) //Some((123, "abc"))

 final case class Cat(name: String, born: Int, color: String)

 println((Option("Garfield"), Option(1978), Option("Orange & black")).mapN(Cat.apply)) //Some(Cat("Garfield", 1978, "Orange & black"))

}
