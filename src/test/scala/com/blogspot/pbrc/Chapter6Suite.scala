package fpinscala.functionalstate

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class Chapter6Suite extends FunSuite {

  import Chapter6._

  case class MockMinInt extends RNG {
    def nextInt: (Int, RNG) =
      (Int.MinValue, this)
  }

  def randDoubles(i: (Double, RNG)): Stream[Double] = i._1 #:: randDoubles(double(i._2))

  test("nonNegativeInt can handle Integer.min") {
    assert(Int.MaxValue === nonNegativeInt(new MockMinInt())._1)
  }

  test("1000 doubles between 0 and 1") {
    every(randDoubles(double(new Simple(42))).take(1000)) should (be >= 0.0 and be <= 1.0)
  }

  test("can generate n random ints") {
    ints(3)(new Simple(30))._1.size should be(3)
  }

  test("Can generate n random ints via sequence") {
    val is = intsViaSequence(30)(new Simple(42))
    println(is._1)
    is._1.size should be(30)

  }

}

