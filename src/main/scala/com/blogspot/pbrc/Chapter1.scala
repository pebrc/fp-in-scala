package com.blogspot.pbrc

object Chapter1 {

  def fib(n: Int): Int = {

    def recur(c: Int, prev: Int, cur: Int): Int = {
      if (c == n) cur
      else recur(c + 1, cur, cur + prev)
    }
    recur(1, 0, 1)
  }

  def fib2(n: Int) = {

    def series(i: Int, j: Int): Stream[Int] = i #:: series(j, i + j)

    series(1, 1).take(n).last
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n + 1 >= as.length) true
      else if (!gt(as(n), as(n + 1))) false
      else loop(n + 1)
    }
    loop(0)
  }

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => partial(a, f)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

}
