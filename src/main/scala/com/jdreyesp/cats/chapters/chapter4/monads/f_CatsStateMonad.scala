package com.jdreyesp.cats.chapters.chapter4.monads

/**
 * cats.data.State allows us to pass additional state around as part of a com‐
 * putation. We define State instances representing atomic state operations
 * and thread them together using map and flatMap. In this way we can model
 * mutable state in a purely functional way, without using actual mutation.
 *
 * Note: Results of sequential computations of State are using Eval monad, so they are stack safe
 */
object CatsStateMonad extends App {

  import cats.data.State
  import State._

  // This represents a Int state with a String result
  val step1 = State[Int, String] { stateAsNum =>
    val newStateAsNum = stateAsNum + 1
    (newStateAsNum, s"Result of step1: $newStateAsNum")
  }

  val step2 = State[Int, String] { stateAsNum =>
    val newStateAsNum = stateAsNum * 2
    (newStateAsNum, s"Result of step2: $newStateAsNum")
  }

  private val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  println(both.run(0).value)

  // Multiple computations using the utility methods get, set, modify and inspect
  // get: Creates a State
  // set: Creates a State and sets the state value
  // modify: Creates a State and receives a function that will be applied in the state part
  // inspect: Creates a State and receives a function that will be applied in the result part
  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  val (state, result) = program.run(1).value
  println(s"$state - $result")

}
