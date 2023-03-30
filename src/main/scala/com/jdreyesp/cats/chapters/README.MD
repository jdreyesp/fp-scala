## Explanation abou the MonadTypeClass hierarchy

Because of the lawful nature of the relationships between the type classes,
the inheritance relationships are constant across all instances of a type class.

Apply defines product in terms of ap and map; Monad defines product, ap,
and map, in terms of pure and flatMap.
To illustrate this let’s consider two hypothetical data types:

- `Foo` is a monad. It has an instance of the Monad type class that imple‐
ments pure and flatMap and inherits standard definitions of product,
map, and ap;
- `Bar` is an applicative functor. It has an instance of Applicative that
implements pure and ap and inherits standard definitions of product
and map.

What can we say about these two data types without knowing more about
their implementation?

We know strictly more about Foo than Bar: Monad is a subtype of
Applicative, so we can guarantee properties of Foo (namely flatMap) that
we cannot guarantee with Bar. Conversely, we know that Bar may have a
wider range of behaviours than Foo. It has fewer laws to obey (no flatMap),
so it can implement behaviours that Foo cannot.
This demonstrates the classic trade‐off of power (in the mathematical sense)
versus constraint. The more constraints we place on a data type, the more
guarantees we have about its behaviour, but the fewer behaviours we can
model.

Monads happen to be a sweet spot in this trade‐off. They are flexible enough
to model a wide range of behaviours and restrictive enough to give strong guar‐
antees about those behaviours. However, there are situations where monads
aren’t the right tool for the job. Sometimes we want Thai food, and burritos
just won’t satisfy.

Whereas monads impose a strict sequencing on the computations they model,
applicatives and semigroupals impose no such restriction. This puts them in a
different sweet spot in the hierarchy. We can use them to represent classes
of parallel / independent computations that monads cannot.
We choose our semantics by choosing our data structures. If we choose a
monad, we get strict sequencing. If we choose an applicative, we lose the
ability to flatMap. This is the trade‐off enforced by the consistency laws. So
choose your types carefully!