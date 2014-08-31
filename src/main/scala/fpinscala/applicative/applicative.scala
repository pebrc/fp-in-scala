import java.util.Date

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {

  self =>

  // primitive combinators
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    ofa.foldRight(unit(Map[K, V]()))((kv, fa) => map2(fa, kv._2)((acc, v) => acc.+(kv._1 -> v)))
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    sequence(List.fill(n)(fa))
  }
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def map3[A, B, C, D](fa: F[A],
    fb: F[B],
    fc: F[C])(f: (A, B, C) => D): F[D] = {
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }

  def map4[A, B, C, D, E](fa: F[A],
    fb: F[B],
    fc: F[C],
    fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  }

  def product[G[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = new Applicative[({ type f[x] = (F[x], G[x]) })#f] {

    override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
      (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))

    override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

  }

  def compose[G[_]](G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] = new Applicative[({ type f[x] = F[G[x]] })#f] {

    override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
      self.map2(fa, fb)((a: G[A], b: G[B]) => G.map2(a, b)(f))

    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

  }

}

trait Applicative2[F[_]] extends Functor[F] {

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def map3[A, B, C, D](fa: F[A],
    fb: F[B],
    fc: F[C])(f: (A, B, C) => D): F[D] = {
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }

  def map4[A, B, C, D, E](fa: F[A],
    fb: F[B],
    fc: F[C],
    fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  }

}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {

  def validation[E] = new Applicative[({ type f[x] = Validation[E, x] })#f] {

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Failure(h, t), Failure(h2, t2)) => Failure(h, (h2 +: t) ++ t2)
        case (Success(_), f @ Failure(_, _)) => f
        case (f @ Failure(_, _), Success(_)) => f
        case (Success(a), Success(b)) => Success(f(a, b))
      }

    override def unit[A](a: => A): Validation[E, A] = Success(a)

  }

  val stringWebForm = validation[String]

  case class WebForm(name: String, birthdate: Date, phoneNumber: String)

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case _: Exception => Failure("Birthdate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
    stringWebForm.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone))(
        WebForm(_, _, _))

}