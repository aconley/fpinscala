package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // Adds a tag to any failure messages
  def tag(msg: String): Prop = {
    def fn(maxsz: MaxSize, n: TestCases, rng: RNG) =
      run(maxsz, n, rng) match {
        case Falsified(fl, succ) => Falsified(msg + ": " + fl, succ)
        case other => other
      }
    new Prop(fn)
  }

  // Combines two tests using short-circuiting and
  def &&(p: Prop): Prop = {
    def fn(maxsz: MaxSize, n: TestCases, rng: RNG) =
      run(maxsz, n, rng) match {
        case Passed | Proved => p.run(maxsz, n, rng) // This passed, try the other
        case other => other
      }
    new Prop(fn)
  }

  // Combineds two tests using short circuiting or
  def ||(p: Prop): Prop = {
    def fn(maxsz: MaxSize, n: TestCases, rng: RNG) =
      run(maxsz, n, rng) match {
        case Falsified(msg, _) =>
          val newp = p.tag(msg) // Append failure message
          newp.run(maxsz, n, rng) // Then try other one
        case other => other
      }
    new Prop(fn)
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result { def isFalsified = false }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result { def isFalsified = false }

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: %s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace: \n ${e.getStackTrace.mkString("\n")}"

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  // Helper function for running tests
  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

}

object Gen {
  // Generator that always returns a
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  // Generate a Boolean
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  // Generate an int
  def int: Gen[Int] = Gen(State(RNG.int))

  // Generates a random int in [start, stopExclusive)
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val rng = stopExclusive - start
    val st = State(RNG.nonNegativeInt) map (n => start + n % rng)
    Gen(st)
  }

  // Generate a double
  def double: Gen[Double] = Gen(State(RNG.double))

  // Generate list of unspecified size, given something
  //  that can generate A's
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  // Generate list of unspecified size > 1, given something
  //  that can generate A's
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  // Generate a list of a specified size, given something that
  //  can generate A's
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    // Take a list of states, sequence them to a state of list
    val lrand = List.fill(n)(g.sample)
    // Wrap in a gen
    Gen(State.sequence(lrand))
  }

  // A generator that randomly selects outputs from g1, g2
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  // A generator that randomly selects outputs from g1, g2
  //  but with user specified non-negative weights
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    assume(g1._2 >= 0, "Weight of g1 must be non-negative")
    assume(g2._2 >= 0, "Weight of g2 must be non-negative")
    assume(g1._2 + g2._2 > 0, "Sum of weights must be positive")
    val prob1 = g1._2 / (g1._2 + g2._2)
    double flatMap (d => if(d > prob1) g1._1 else g2._1)
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  // These are just exercises in wrapping and unwrapping
  //  states and using the state functions we created earlier
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  // Generate list of specified size
  def listOfN(n: Int): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(this.sample)))

  // Utility function to convert to a SGen that ignores the size argument
  def unsized: SGen[A] = SGen[A](_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  // Utility functions shared with Gen -- easiest to just delegate
  //  using function composition (e.g., call forSize to get the Gen,
  //  then call Gen map (or whatever), and that function is the argument
  //  to SGen)
  def map[B](f: A => B): SGen[B] = SGen[B](forSize andThen (_ map f))
  def map2[B, C](g: SGen[B])(f: (A, B) => C): SGen[C] =
    SGen[C](n => forSize(n).map2(g.apply(n))(f))
  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen[B](forSize andThen (_ flatMap f))
}

// Some actual test cases
object testCases {
  val smallInt = Gen.choose(-10, 10)

  val maxProp1 = forAll(listOf1(smallInt)){ ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
  val maxProp2 = forAll(listOf1(smallInt)){ ns =>
    ns.contains(ns.max)
  }

  val isSortedProp1 = forAll(listOf(smallInt)){ list: List[Int] =>
    def isOrdered(l: List[Int]): Boolean = {
      !l.zip(l.tail).exists{ x => x._1 > x._2}
    }
    val sorted_list = list.sorted
    // Either empty, has one element, or is in order
    list.isEmpty || list.tail.isEmpty || isOrdered(list)
  }
}



