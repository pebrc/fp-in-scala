package fpinscala.fp

import Chapter1._
import org.scalatest.FunSuite

class Chapter1Suite extends FunSuite {

  test("The first eigth fibonacci numbers are 1 1 2 3 5 8 13 21") {
    assert(1 == fib(1))
    assert(1 == fib(2))
    assert(2 == fib(3))
    assert(3 == fib(4))
    assert(5 == fib(5))
    assert(8 == fib(6))
    assert(13 == fib(7))
    assert(21 == fib(8))
  }

  test("Using streams: The first eigth fibonacci numbers are 1 1 2 3 5 8 13 21") {
    assert(1 == fib2(1))
    assert(1 == fib2(2))
    assert(2 == fib2(3))
    assert(3 == fib2(4))
    assert(5 == fib2(5))
    assert(8 == fib2(6))
    assert(13 == fib2(7))
    assert(21 == fib2(8))
  }

  test("Being sorted means beeing monotonic") {
    assert(true === isSorted(Array(1, 2, 3, 4), (x: Int, y: Int) => x < y))
    assert(false === isSorted(Array(1, 2, 3, 4, 3), (x: Int, y: Int) => x < y))
  }

  test("Currying narrows the argument list") {
    def add = (x: Int, y: Int) => x + y
    def curried = curry(add)
    def onePlus = curried(1)
    assert(3 == onePlus(2))
  }

  test("Uncurrying reverses a curry") {
    def add = (x: Int, y: Int) => x + y
    def curried = curry(add)
    assert(4 == uncurry(curried)(2, 2))
  }

  test("The result of composition is identical to applying one fun to another") {
    def abs = (x: Int) => if (x < 0) -x else x
    def subFromTwo = (x: Int) => 2 - x
    assert(0 == compose(subFromTwo, abs)(-2))
  }

}
