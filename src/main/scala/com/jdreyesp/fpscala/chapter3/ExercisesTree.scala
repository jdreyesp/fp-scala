package com.jdreyesp.fpscala.chapter3

import com.jdreyesp.fpscala.chapter3.Tree.depth

sealed trait Tree[+A]
case class Leaf[+A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree extends App {

  def size[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)(_ + _)
  }

  def maximum(tree: Tree[Int]): Int = {
    fold(tree)(l => l.value)((x, y) => x.max(y))
  }

  def depth[A](tree: Tree[A]): Int = {

    def depthAcc[A](tree: Tree[A], acc: Int): Int = {
      tree match {
        case Leaf(_) => acc
        case Branch(left, right) => depthAcc(left, acc + 1).max(depthAcc(right, acc + 1))
      }
    }

    depthAcc(tree, 1)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def mapWithReduce[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold[A, Tree[B]](tree)(l => Leaf(f(l.value)))((left, right) => Branch(left, right))
  }

  def fold[A, B](tree: Tree[A])(init: Leaf[A] => B)(f: (B, B) => B): B = {
    tree match {
      case leaf @ Leaf(_) => init(leaf)
      case Branch(left, right) => f(fold(left)(init)(f), fold(right)(init)(f))
    }
  }

  val t = Branch(Branch(Leaf(1),Leaf(2)), Leaf(3))
  println(size(t))
  println(maximum(t))
  println(depth(t))
  println(map(t)(value => value + 1))
  println(mapWithReduce(t)(value => value + 1))
}