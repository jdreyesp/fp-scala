package com.jdreyesp.cats.chapters.chapter7.foldabletraverse

/** Foldable abstracts the familiar foldLeft and foldRight operations; */
/** Traverse is a higher‐level abstraction that uses Applicatives to it‐
erate with less pain than folding */
object CatsFoldable extends App {

  def foldLeftList[A](list: List[A]): List[A] = {
    list.foldLeft(List[A]())((acc, item) => item :: acc)
  }

  def foldRightList[A](list: List[A]): List[A] = {
    list.foldRight(List[A]())((item, acc) => item :: acc)
  }

  println(foldLeftList(List(1,2,3)))
  println(foldRightList(List(1,2,3)))
}
