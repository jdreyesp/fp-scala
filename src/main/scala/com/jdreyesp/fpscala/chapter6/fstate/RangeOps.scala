package com.jdreyesp.fpscala.chapter6.fstate

import com.jdreyesp.fpscala.chapter6.fstate.RNG.{double, double3, doubleInt, intDouble}

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

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (Int.MinValue, nextRNG) => (Int.MaxValue, nextRNG)
      case (n, nextRNG) if n < 0 => (-n, nextRNG)
      case (n, nextRNG) if n >= 0 => (n, nextRNG)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = RNG.nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nInt, nextRNG) = nonNegativeInt(rng)
    val (nDouble, _) = double(rng)
    ((nInt, nDouble), nextRNG)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((nInt, nDouble), nextRNG) = intDouble(rng)
    ((nDouble, nInt), nextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (nDouble1, rng2) = double(rng)
    val (nDouble2, rng3) = double(rng2)
    val (nDouble3, nextRNG) = double(rng3)
    ((nDouble1, nDouble2, nDouble3), nextRNG)
  }

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
}

object Main extends App {

  val rng = SimpleRNG(42)
  val (n, rng2) = rng.nextInt
  println(n)
  println(rng2.nextInt)

  println(RNG.nonNegativeInt(rng))
  println(double(double(RNG.double(rng)._2)._2))

  println(intDouble(rng))
  println(doubleInt(rng))
  println(double3(rng))

  println(RNG.ints(5)(rng))
}
