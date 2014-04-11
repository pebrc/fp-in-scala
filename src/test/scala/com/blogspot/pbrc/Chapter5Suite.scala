package fpinscala.laziness

import org.scalatest.FunSuite

class Chapter5Suite extends FunSuite {

  test("A stream of 20000 numbers does not overflow") {
    assert(List(1 to 20000: _*) === Stream(1 to 20000: _*).toList)
  }

  test("Can take 10 out of stream of 20 numbers") {
    assert(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) === Stream(1 to 20: _*).take(10).toList)
  }

  test("Can take 20000 out of a stream ") {
    assert(List(1 to 20000: _*) === Stream(1 to 30000: _*).take(20000).toList)

  }

  test("Dropping removes n elements from the front") {
    assert(List(2, 3) == Stream(1, 2, 3).drop(1).toList)
  }

  test("takeWhile returns the longest prefix of a stream that satisfies the predicate") {
    assert(List(1, 2, 3) === Stream(1 to 100: _*).takeWhile(_ < 4).toList)
  }

  test("forAll tests whether a predicate holds for all elements of this stream") {
    assert(false === Stream(1 to 100: _*).forAll(_ % 2 == 0))
    assert(true === Stream(1 until 100: _*).forAll(_ < 100))
  }

}
