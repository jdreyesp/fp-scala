package com.jdreyesp.fpscala.chapter7.fparallelism

import com.jdreyesp.fpscala.chapter7.fparallelism.Par.{Par, asyncF, fork, logger, map2, parFilter, parMap, unit}
import org.slf4j.LoggerFactory

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}

case class UnitFuture[A](get: A) extends Future[A] {
  override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  override def isCancelled: Boolean = false
  override def isDone: Boolean = true
  override def get(timeout: Long, unit: TimeUnit): A = {
    logger.info("Executing operation on the same thread")
    get
  }
}

object Par {

  val logger = LoggerFactory.getLogger("Par")

  type Par[A] = ExecutorService => Future[A]

  /**
   * Promotes a constant value to a parallel computation
   * @param a
   * @tparam A
   * @return
   */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  /**
   * High order function that executes two Par[A] (in parallel or not, decided by the programmer) and
   * apply a function to the two returned computations.
   * @param l
   * @param r
   * @param f
   * @tparam A
   * @return
   */
  def map2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get(), bf.get()))
    }

  /**
   * Fork creates a new computation in a separate thread. This is meant to be here for making the parallel computation call explicit
   * @param f
   * @tparam A
   * @return
   */
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => {
    logger.info("Executing operation on a separate thread")
    a(es).get()
  })

  /**
   * Lazy marks the the parameter function to a parallel computation
   * @param a
   * @tparam A
   * @return
   */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
   * Runs the parallel computation and give the return value back
   * @param a
   * @tparam A
   * @return
   */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /**
   * Returns an asynchronous version of the input function
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def asyncF[A,B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))


  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    (es: ExecutorService) => UnitFuture(ps.foldLeft(List[A]())((b, parA) => b :+ parA(es).get()))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork { val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    (e: ExecutorService) => {
      val parallelFilteredItems: Par[List[Boolean]] = parMap[A, Boolean](as)(f)
      UnitFuture(as.zip(parallelFilteredItems(e).get()).filter{ case (_, b) => !b }.map(_._1))
    }
  }

}

object Library extends App {

  val logger = LoggerFactory.getLogger("Library")

  val executorService: ExecutorService = Executors.newFixedThreadPool(3)

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    ints match {
      case Seq(a: Int) => unit(a)
      case head +: tail => map2(unit(head), sum(tail))(_ + _)
    }
  }

  logger.info("SUM: {}", sum(IndexedSeq.range(1, 4))(executorService).get(1000L, TimeUnit.MILLISECONDS))
  logger.info("UNIT: {}", unit(15)(executorService).get(1000L, TimeUnit.MILLISECONDS))
  logger.info("PARALLEL: {}", fork(unit(15))(executorService).get())

  val par1: Par[Int] = _ => UnitFuture { Thread.sleep(1000); 15 }
  val par2: Par[Int] = _ => UnitFuture { Thread.sleep(3000); 10 }

  val init = System.currentTimeMillis()
  logger.info("MAP2 WAITING RESULT: {}", map2(par1, par2)(_ + _)(executorService).get())
  logger.info("MAP2 took: {} ms", System.currentTimeMillis() - init)

  def sumRange(from: Int): Int = IndexedSeq.range(from, from+5).reduce(_ + _)
  logger.info("ASYNCF: {}", asyncF(sumRange)(10)(executorService).get(1000L, TimeUnit.MILLISECONDS))

  logger.info("PARMAP: {}", parMap(List(1, 2, 3))(_ + 1)(executorService).get())

  logger.info("FILTER: {}", parFilter(List(1, 2, 3))(_ == 1)(executorService).get())

  sys.exit(0)
}
