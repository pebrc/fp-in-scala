package fpinscala.functionalstate

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class Chapter6Suite extends FunSuite {

  import RNG._

  case class MockMinInt(always: Int) extends RNG {
    def nextInt: (Int, RNG) =
      (always, this)
  }

  def randDoubles(i: (Double, RNG)): Stream[Double] = i._1 #:: randDoubles(double(i._2))

  test("nonNegativeInt can handle Integer.min") {
    assert(Int.MaxValue === nonNegativeInt(new MockMinInt(Int.MinValue))._1)
  }

  test("1000 doubles between 0 and 1 excluding") {
    every(randDoubles(double(new Simple(42))).take(1000)) should (be >= 0.0 and be < 1.0)
  }

  test("double excludes 1") {
    double(new MockMinInt(Int.MaxValue))._1 should be < (1.0)
  }

  test("can generate n random ints") {
    ints(3)(new Simple(30))._1.size should be(3)
  }

  test("Can generate n random ints via sequence") {
    val is = intsViaSequence(30)(new Simple(42))
    is._1.size should be(30)
  }

}

