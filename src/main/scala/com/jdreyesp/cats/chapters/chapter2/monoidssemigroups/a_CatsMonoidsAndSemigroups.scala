package com.jdreyesp.cats.chapters.chapter2.monoidssemigroups

/**
  Formally, a monoid for a type A is:
  • an operation combine with type (A, A) => A
  • an element empty of type A
In addition to providing the combine and empty operations, monoids must
formally obey several laws. For all values x, y, and z, in A, combine must be
associative and empty must be an identity element

  Cats provides syntax for the combine method in the form of the |+| operator.
Because combine technically comes from Semigroup, we access the syntax
by importing from `cats.syntax.semigroup`.


 A semigroup is just the combine part of a monoid, without the empty part.
While many semigroups are also monoids, there are some data types for which
we cannot define an empty element. For example, we have just seen that
sequence concatenation and integer addition are monoids. However, if we
restrict ourselves to non‐empty sequences and positive integers, we are no
longer able to define a sensible empty element. Cats has a NonEmptyList
data type that has an implementation of Semigroup but no implementation
of Monoid.
 */
object CatsMonoidsAndSemigroups extends App {

  import cats.Monoid
  import cats.instances.string._

  import cats.Semigroup
  import cats.syntax.semigroup._

  println(Monoid[String].combine("Hi ", "there"))
  println(Monoid[String].empty)

  println(Semigroup[String].combine("Hi ", "there"))
  println("Hi " |+| "there" |+| Monoid[String].empty)
}
