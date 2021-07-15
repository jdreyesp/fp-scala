package com.jdreyesp.fpscala.chapter6.fstate

import com.jdreyesp.fpscala.chapter6.fstate.State.{double, double3, doubleInt, intDouble, ints, nonNegativeInt}

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class State[S, +A](run: S => (A, S))

object State {

  type Rand[+A] = State[RNG, A]

  val nonNegativeInt: Rand[Int] = { State(rng =>
    rng.nextInt match {
      case (Int.MinValue, nextRNG) => (Int.MaxValue, nextRNG)
      case (n, nextRNG) if n < 0 => (-n, nextRNG)
      case (n, nextRNG) if n >= 0 => (n, nextRNG)
    })
  }

  def double: Rand[Double] = mapFromFlatMap(nonNegativeInt)(_.toDouble / Int.MaxValue)

  def intDouble: Rand[(Int, Double)] = map2(nonNegativeInt, double)((_, _))

  def doubleInt: Rand[(Double, Int)] = map2(double, nonNegativeInt)((_, _))

  def double3: Rand[(Double, Double, Double)] = map3(double, double, double)((_, _, _))

  //TODO: pending
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    def intR(n: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
      n match {
        case 0 => (acc, rng)
        case nPos if nPos > 0 => {
          val (n, nextRNG) = rng.nextInt
          intR(nPos - 1, acc :+ n, nextRNG)
        }
        case _ => throw new IllegalArgumentException("Negative number not allowed")
      }
    }

    intR(count, List(), rng)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    State(rng => {
      val (a, rng2) = s.run(rng)
      (f(a), rng2)
    })
  }

  def map2[A, B, C](s1: Rand[A], s2: Rand[B])(f: (A, B) => C): Rand[C] = {
    State(rng => {
      val (a, rng2) = s1.run(rng)
      val (b, _) = s2.run(rng)
      (f(a, b), rng2)
    })
  }

  def map3[A, B, C, D](s1: Rand[A], s2: Rand[B], s3: Rand[C])(f: (A, B, C) => D): Rand[D] = {
    State(rng => {
      val (a, rng2) = s1.run(rng)
      val (b, _) = s2.run(rng)
      val (c, _) = s3.run(rng)
      (f(a, b, c), rng2)
    })
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    State(rng => {
      val (a, rng2) = f.run(rng)
      val (b, rng3) = g(a).run(rng2)
      (b, rng3)
    })
  }

  def mapFromFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => State(rng => (f(a), rng)))
}

object Main extends App {

  val rng = SimpleRNG(42)
  val (n, rng2) = rng.nextInt
  println(n)
  println(rng2.nextInt)

  println(nonNegativeInt.run(rng))

  println(double.run(rng))

  println(intDouble.run(rng))
  println(doubleInt.run(rng))
  println(double3.run(rng))

  println(ints(5)(rng))
}
