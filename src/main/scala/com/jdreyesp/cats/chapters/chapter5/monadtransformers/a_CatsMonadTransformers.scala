package com.jdreyesp.cats.chapters.chapter5.monadtransformers

/**
 * monad transformers. The combined map and flatMap
methods allow us to use both component monads without having to recur‐
sively unpack and repack values at each stage in the computation.

 So instead of:

 def lookupUserName(id: Long): Either[Error, Option[String]] =
   for {
    optUser <- someFuncThatReturnsUserOption(param)
   } yield for { user <- optUser } yield user.name

 we can:

 val name = EitherT[Error, Option[String]]
 */
object CatsMonadTransformers extends App {

  //Exercise: Monads: Transform and Roll Out (page 140)

}
