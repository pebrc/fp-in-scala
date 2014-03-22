package com.blogspot.pbrc

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs: List[A]): List[A] = xs match {
    case Cons(h, t) => t
    case _ => Nil
  }

  def setHead[A](xs: List[A], head: A): List[A] = xs match {
    case Cons(oldHead, t) => Cons(head, t)
    case _ => Cons(head, Nil)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(h, t) => {
      if (n > 0) drop(t, n - 1)
      else l
    }
    case _ => Nil
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => List()
    case Cons(h, Cons(t, Nil)) => Cons(h, Nil)
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((a, b) => b + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f:(B,A) => B):B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum2(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def reverse[A](l: List[A]):List[A] = foldLeft(l, Nil:List[A])((b, a) => Cons(a, b))

  //after reading: https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/13.hint.txt
  def foldRightTailRec[A, B](as: List[A], z: B)(f: (A,B) => B):B = foldLeft(reverse(as), z)((b, a) => f(a,b))

  def append[A](a1: List[A], a2: List[A]):List[A] = foldRight(a1, a2)((a, a2) => Cons(a, a2)) 
 
}

object Chapter3 {

  import com.blogspot.pbrc.List._

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  val y = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
}
