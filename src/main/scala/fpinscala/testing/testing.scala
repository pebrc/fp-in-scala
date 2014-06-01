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
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(f.andThen(_.sample)))
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

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    //this is the reference solution
    Gen(State.sequence(List.fill(n)(g.sample)))
  }
}
