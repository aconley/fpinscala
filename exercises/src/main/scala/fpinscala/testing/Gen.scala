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

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  def double: Gen[Double] = Gen(State(RNG.double))
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val rng = stopExclusive - start
    val st = State(RNG.nonNegativeInt) map (n => start + n % rng)
    Gen(st)
  }
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    // Take a list of states, sequence them to a state of list
    val lrand = List.fill(n)(g.sample)
    // Wrap in a gen
    Gen(State.sequence(lrand))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    assume(g1._2 >= 0, "Weight of g1 must be non-negative")
    assume(g2._2 >= 0, "Weight of g2 must be non-negative")
    assume(g1._2 + g2._2 > 0, "Sum of weights must be positive")
    val prob1 = g1._2 / (g1._2 + g2._2)
    double flatMap (d => if(d > prob1) g1._1 else g2._1)
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  // These are all just exercises in wrapping and unwrapping
  //  states and using the state functions we created earlier
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(n: Int): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(this.sample)))
}

trait SGen[+A] {

}

