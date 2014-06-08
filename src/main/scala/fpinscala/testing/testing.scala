package fpinscala.testing

import fpinscala.testing.Prop._
import fpinscala.functionalstate.{ Simple, RNG, State }
import fpinscala.laziness.Stream
import scala.Some

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
  type MaxSize = Int
  type TestCases = Int
  type Result = Option[(FailedCase, SuccessCount)]
  type FailedCase = String
  type SuccessCount = Int

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      {
        val casesPerSize = (n + (max - 1) / max)
        val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop = props.map(p => Prop {
          (max, _, rng) => p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
        prop.run(max, n, rng)
      }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      {
        randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
          case (a, i) => try {
            if (f(a)) None else Some((a.toString, i))
          } catch {
            case e: Exception => Some((buildMsg(a, e), i))
          }
        }.find(_.isDefined).getOrElse(None)
      }
  }

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack tracke:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
    maxSize: Int = 100, // A default argument of `200`
    testCases: Int = 100,
    rng: RNG = Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Some((msg, n)) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case None =>
        println(s"+ OK, passed $testCases tests.")
    }
  }

}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case None => p.run(max, n, rng)
        case failure => failure
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case None => p.run(max, n, rng)
        case Some((msg, _)) => p.tag(msg).run(max, n, rng)
      }
  }

  def tag(msg: String): Prop = Prop {
    (max, n, rng) =>
      {
        run(max, n, rng) match {
          case Some((err, cnt)) => Some((msg + "\n" + err, cnt))
          case x => x
        }
      }
  }

};

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(f.andThen(_.sample)))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }

  //reference solution
  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)
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

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n max 1, g))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n, g))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    for {
      first <- boolean
      next <- if (first) g1 else g2
    } yield next
  }

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

}
