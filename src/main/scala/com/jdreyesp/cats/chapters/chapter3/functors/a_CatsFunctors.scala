package com.jdreyesp.cats.chapters.chapter3.functors

import cats.Functor

import scala.util.Try

sealed trait Tree[A]

case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class Leaf[A](value: A) extends Tree[A]

object TreeFunctor {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[X, Y](fa: Tree[X])(f: X => Y): Tree[Y] = {
      fa match {
        case Node(left, right) => Node(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }
    }
  }
}

// Informally, a functor is anything with a `map` method.
// Examples: Option, List, Either, ...
//
// Note: Monads and Applicative Functors are special cases of Functor
//
object CatsFunctors extends App {

  import TreeFunctor._

  // See that we reference Tree without underscores.
  // That's because `Tree` is a type constructor meanwhile `Tree[String]` is a type.
  val treeFunctorInstance = Functor[Tree]

  /*
  for {
    numb2 <- List(1,2,3)
    numb1 <- Some(10)
  } yield numb1 + numb2
   */

  def mapToInt(treeVector: Vector[Tree[String]]): Vector[Tree[Int]] = {
    treeVector
      .map(tree => Try(treeFunctorInstance.map(tree)((str: String) => Integer.parseInt(str))))
      .filter(_.isSuccess)
      .map(_.get)
  }

  val tree1 = Leaf("1")
  val tree2 = Node(Node(Leaf("1"), Leaf("2")), Leaf("3"))
  val tree3 = Leaf("a") //should throw an exception

  println(mapToInt(Vector(tree1, tree2, tree3)))

}
