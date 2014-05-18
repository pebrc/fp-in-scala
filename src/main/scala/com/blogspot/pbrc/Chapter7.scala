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

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sequence[A](l: List[Par[A]]): Par[List[A]] = {
    l.foldRight(unit(List.empty: List[A]))((pcur, pacc) => { map2(pcur, pacc)(_ :: _) })
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val ps: List[Par[A]] = l.map(lazyUnit(_))
    ps.foldRight(unit(List.empty[A]))((pcur, pfiltered) => {
      map2(pcur, pfiltered)((cur, filtered) => {
        if (f(cur)) cur :: filtered
        else filtered
      }
      )
    })
  }

  //as in reference solution
  def parFilter2[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val ps = l.map(asyncF(a => {
      if (f(a)) List(a) else List()
    }))
    map(sequence(ps))(_.flatten)
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es =>
      {
        val index = n(es).get()
        choices(index)(es)
      }
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))
  }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es =>
      {
        choices(pa(es).get)(es)
      }
  }

  def chooseN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    chooser(n)(choices)
  }

  def choose[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    chooser(cond)(b => { if (b) t else f })
  }

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
