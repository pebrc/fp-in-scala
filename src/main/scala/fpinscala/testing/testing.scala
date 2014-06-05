package fpinscala.testing

import fpinscala.testing.Prop.{ FailedCase, SuccessCount }
import fpinscala.functionalstate.{ RNG, State }

/**
 * Exercise 3
 */
//trait Prop {
//  def check: Boolean
//  def &&(p: Prop): Prop = {
//    new Prop {
//      override def check: Boolean = Prop.this.check || p.check
//    }
//  }
//}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop
}

case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(f.andThen(_.sample)))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(i => i + start % (stopExclusive - start)))
  }

  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.nonNegativeInt).map(_ % 2 == 0))
  }

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = {
    for {
      first <- choose(from, to)
      second <- choose(from, to - 1) //make sure we are still in range when we have to change parity
      // If x and y are two integers for which x+y is even, then x and y have the same parity
    } yield (first, if ((first + second) % 2 == 0) second else second + 1)
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    //this is the reference solution
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    for {
      first <- boolean
      next <- if (first) g1 else g2
    } yield next
  }

}
