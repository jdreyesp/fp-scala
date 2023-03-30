package com.jdreyesp.cats.effects

import cats.effect.{ExitCode, IO, IOApp}

/**
 * In functional programming, an effect is any observable change that occurs as a
 * result of running a program. This includes things like reading from or writing to a database,
 * making a network request, printing to the console, or updating a UI.
 *
 * Effects are often modeled as data types or abstractions that encapsulate the effectful
 * behavior of a program. For example, in Scala, the IO monad is often used to represent effectful
 * computations that can perform IO operations, while in Haskell, the IO monad is used for similar purposes.
 *
 * By modeling effects in this way, functional programming languages provide a way to reason
 * about and compose effectful programs in a purely functional way.
 * This means that effects are treated as values that can be transformed,
 * combined, and manipulated like any other value in the language,
 * rather than being treated as side effects that can modify the state of the program in unpredictable ways.
 *
 * Effectful programming is often contrasted with pure functional programming,
 * which aims to eliminate effects entirely by only allowing functions to compute results
 * based on their inputs, without performing any observable effects on the outside world.
 * While pure functional programming is an ideal to strive for, it is often necessary to perform
 * effectful operations in real-world programs, and functional programming provides powerful
 * abstractions for dealing with these effects in a composable and maintainable way.
 */
object CatsEffectsIO extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
      for {
        _ <- IO(println("What's your name?"))
        name <- IO(scala.io.StdIn.readLine())
        _ <- IO(println(s"Hello, $name!"))
      } yield ExitCode.Success
    }
}
