package fpinscala.parallelism

import java.util.concurrent._

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /*Ex 1*/
  /* def  map2[A, B, C](left: Par[A], right: Par[B])((A, B) => C ):
  Par[C] */

  case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] =
    (es: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map2WithTimeouts[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A] (a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  case class Map2Future[A, B, C](af: Future[A], bf: Future[B], f: (A, B) => C) extends Future[C] {

    var cache: Option[C] = None

    def isDone = cache.isDefined

    def cancel(evenIfRunning: Boolean) = af.cancel(evenIfRunning) || bf.cancel(evenIfRunning)
    def isCancelled = af.isCancelled || bf.isCancelled

    def get = compute(Long.MaxValue)

    def get(timeout: Long, units: TimeUnit) = compute(TimeUnit.MILLISECONDS.convert(timeout, units))

    private def compute(timeoutInMillis: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.currentTimeMillis
        val a = af.get(timeoutInMillis, TimeUnit.MILLISECONDS)
        val remainingTime = timeoutInMillis - (System.currentTimeMillis - start)
        val b = bf.get(remainingTime, TimeUnit.MILLISECONDS)
        cache = Some(f(a, b))
        cache.get
    }

  }

}
