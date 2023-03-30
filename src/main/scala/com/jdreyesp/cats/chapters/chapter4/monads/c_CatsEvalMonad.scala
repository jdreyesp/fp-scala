package com.jdreyesp.cats.chapters.chapter4.monads

import cats.Eval

//Call-by-value Eval.now
//Call-by-name Eval.always
//Call-by-need Eval.later
object CatsEvalMonad extends App {

  val now = Eval.now(math.random() + 1000)
  val always = Eval.always(math.random() + 3000)
  val later = Eval.later(math.random() + 2000)

  val triplet: Eval[(Double, Double, Double)] = for {
    nowValue <- now
    alwaysValue <- always
    laterValue <- later
  } yield (nowValue, alwaysValue, laterValue)

  //Evaluation happens here
  println(triplet.value)

  //Trampoline for avoiding StackOverflowError (stack-safe)
  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      // Here, instead of using factorial(n-1).map(_ * n) we defer its execution
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  println(factorial(50000).value) // a very long number
  //This depends now on the heap size (since it creates a chain of function objects on the heap)
}
