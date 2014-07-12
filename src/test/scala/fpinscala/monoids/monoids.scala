package fpinscala.monoids

import fpinscala.functionalstate.{ Simple, RNG }
import org.scalatest.FunSuite
import fpinscala.testing._
import fpinscala.testing.Prop._

import fpinscala.monoids.Monoid._

/**
 * Created by p_brc on 09/07/2014.
 */
class MonoidSuite extends FunSuite {

  def monoidLaws[A](m: Monoid[A])(l: List[A]): Boolean = l match {
    case a1 :: a2 :: a3 :: Nil => {
      (m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a2, a3))) &&
        (m.op(a1, m.zero) == m.op(m.zero, a1))
    }
    case _ => false

  }

  test("intAddition forms a monoid") {
    run(forAll(Gen.listOfN(3, Gen.smallInt))(monoidLaws(intAddition)))
  }

  test("intMultiplication forms a monoid") {
    run(forAll(Gen.listOfN(3, Gen.smallInt))(monoidLaws(intMultiplication)))
  }

  test("booleanOr forms a monoid") {
    run(forAll(Gen.listOfN(3, Gen.boolean))(monoidLaws(booleanOr)))
  }

  test("booleanAnd forms a monoid") {
    run(forAll(Gen.listOfN(3, Gen.boolean))(monoidLaws(booleanAnd)))
  }

  test("optionMonoid is monoidal") {
    val gen = Gen.listOfN(3, Gen.choose(-10, 10).map(i => if (i < 0) None else Some(i)))
    run(forAll(gen)(monoidLaws(optionMonoid[Int])))
  }

  //  test("endoMonoid is monoidal") {
  //    val gen = Gen.listOfN(3, Gen.staticfn[Int, Int](Gen.smallInt))
  //    run(forAll(gen)(monoidLaws(endoMonoid[Int])))
  //  }

  def run(p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Some((msg, n)) =>
        fail(s"! Falsified after $n passed tests:\n $msg")
      case None =>
        info(s"+ OK, passed $testCases tests.")
    }
  }

}
