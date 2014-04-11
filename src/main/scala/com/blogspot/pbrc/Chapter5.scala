package fpinscala.laziness

sealed trait Stream[+A] {
  def toList: List[A] = {
    def recur(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => recur(t(), h() :: acc)
      case _ => acc
    }
    recur(this, List()).reverse

  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n != 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n != 0 => t().drop(n - 1)
    case _ => this
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((h, t) => p(h) || t)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((h, t) => p(h) && t)

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) => if (p(h)) Stream.cons(h, t) else Empty)

  def headOption: Option[A] = foldRight(None: Option[A])((h, t) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => Stream.cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)

  def append[B >: A](rest: Stream[B]): Stream[B] =
    foldRight(rest)((h, t) => Stream.cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => f(h).append(t))

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def series(i: Int, j: Int): Stream[Int] = Stream.cons(i, series(j, i + j))
    series(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((v, s)) => Stream.cons(v, unfold(s)(f))
      case _ => Empty
    }

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
