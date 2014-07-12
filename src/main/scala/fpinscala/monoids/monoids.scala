package fpinscala.monoids

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = List()
  }

  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {

    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1.andThen(a2)

    override def zero: (A) => A = identity
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }

  // from the reference solution
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = {
    //type system does not tell us that this is operating from left to right
    // in the wrong way ...
    //unless you reverse the monoid via dual
    foldMap(as, dual(endoMonoid[B]))(f.curried).apply(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    //also not that the operation of endoMonoid is implicit
    //no type hints what it does. I used andThen instead of
    //compose so its order of operation is correct for foldLeft
    foldMap(as, endoMonoid[B])(a => b => f(b, a)).apply(z)
  }

  def foldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    as.length match {
      case 0 => m.zero
      case 1 => f(as(0))
      case x => {
        val (l, r) = as.splitAt(x / 2)
        m.op(foldMap(l, m)(f), foldMap(r, m)(f))
      }
    }
  }

}