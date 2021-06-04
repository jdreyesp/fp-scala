package com.jdreyesp.fpscala.chapter4

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(value) => Some(f(value))
      case None => None
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(value) => value
      case None => default
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f) getOrElse None
  }

  //Alternative implementation
//  def flatMap[B](f: A => Option[B]): Option[B] = {
//    this match {
//      case Some(value) => f(value)
//      case None => None
//    }
//  }


  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse ob
  }

  //Alternative implementation
//  def orElse[B >: A](ob: => Option[B]): Option[B] = {
//    this match {
//      case op @ Some(_) => op
//      case None => ob
//    }
//  }

  def filter(f: A => Boolean): Option[A] = {
   flatMap(value => if(f(value)) Some(value) else None)
  }

  //Alternative implementation
//  def filter(f: A => Boolean): Option[A] = {
//    this match {
//      case Some(value) if !f(value) => None
//      case Some(value) => Some(value)
//      case None => None
//    }
//  }

}

object ExercisesOption extends App {
  val o1: Option[Int] = Some(1)
  val o2: Option[Int] = None

  def variance(xs: Seq[Double]): Option[Double] = {
    val mean = xs.reduce(_ + _) / xs.size
    def applyVariance(value: Double): Double = math.pow(value - mean, 2)

    xs.map(elem => Some(elem).map(elem => applyVariance(elem))).reduce((elem1, elem2) => elem1.flatMap(e1 => elem2.flatMap(e2 => Some(e1 + e2))))
      .flatMap(value => Some(value / xs.size))
  }

  //This is the essence of the for{} comprehension (syntactic sugar) in Scala
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(a1 => b.map(a2 => f(a1, a2)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    Some(a.foldLeft(List[A]())((l, op) => op match { case Some(value) => l :+ value ; case None => l}))
  }

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((op, l) => map2(f(op), l)(_ :: _))


  println(o1.map(value => value + 1))
  println(o1.flatMap(value => Some(value + 1)))
  println(o1.getOrElse(2))
  println(o2.getOrElse(2))
  println(o1.orElse(Some(2)))
  println(o2.orElse(Some(3)))
  println(o1.filter(_ != 1))
  println(o1.filter(_ != 2))

  println(o1.map(value => value + 1).filter(_ == 2).getOrElse(10))
  println(o1.map(value => value + 1).filter(_ == 1).getOrElse(10))

  println(variance(Seq(1D, 2D, 3D)))

  println(map2(o1, o2)((a, b) => a + b))
  println(map2(o2, o1)((a, b) => a + b))
  println(map2(o2, o2)((a, b) => a + b))
  println(map2(o1, Some(1))((a, b) => a + b))

  println(sequence(List(Some(1), Some(2), None, None, Some(3), None)))
  println(traverse(List(1, 2, 3))(a => Some(a + 1)))
}
