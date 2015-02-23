package fpinscala.state

import scala.collection.mutable.ListBuffer

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    // The issue here is that Int.MinValue is
    // smaller than - Int.MaxValue
    // Also, if we mapped only 0 to zero it would
    //  end up under-represented.
    // This deals with both issues.
    val (v, r) = rng.nextInt
    if (v < 0) (-(v + 1), r) else (v, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt(rng)
    (v / (Int.MaxValue.toDouble + 1), r)
  }

  def boolean(rng: RNG): (Boolean, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i < 0, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    require(count >= 0, "count can't be negative")
    def intsInner(n: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = n match {
      case 0 => (xs, r)
      case _ => val (x, newr) = r.nextInt
        intsInner(n - 1, newr, x :: xs)
    }
    intsInner(count, rng, List())
  }

  def doubleRand: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (v1, r1) = ra(rng)
      val (v2, r2) = rb(r1)
      (f(v1, v2), r2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    // Obviously this can also be done with folds
    def seqInner(gs: List[Rand[A]], r: RNG, acc: ListBuffer[A]): (List[A], RNG) =
      gs match {
        case Nil => (acc.toList, r)
        case x :: xs =>
          val (newval, newr) = x(r)
          seqInner(gs.tail, newr, acc += newval)
      }
    rng => seqInner(fs, rng, ListBuffer[A]())
  }

  def intsViaSeq(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (newval, newr) = f(rng)
      g(newval)(newr)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if ((i + (n - 1) - mod) >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = {
    @annotation.tailrec
    def seqI(gs: List[State[S, A]], prevS: S, acc: ListBuffer[A]): (List[A], S) =
      gs match {
        case Nil => (acc.toList, prevS)
        case st :: sts =>
          val (newV, newS) = st.run(prevS)
          seqI(sts, newS, acc += newV)
      }
    State((s: S) => seqI(ss, s, ListBuffer[A]()))
  }

  // Only returns the last value
  def chain[S, A](ss: List[State[S, A]]): State[S, A] = {
    @annotation.tailrec
    def chainI(gs: List[State[S, A]], prevS: S): (A, S) = gs match {
      case Nil => sys.error("Can't run on Nil state")
      case g :: Nil => g.run(prevS)
      case st :: sts =>
        val (newV, newS) = st.run(prevS)
        chainI(sts, newS)
    }
    State((s: S) => chainI(ss, s))
  }
}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  import State.chain
  def CoinState: State[Machine, (Int, Int)] =
    State(s => s match {
      case Machine(true, can, coin) if can > 0 => ((can, coin + 1), Machine(false, can, coin+1))
      case Machine(false, can, coin) => ((can, coin), Machine(false, can, coin))
      case Machine(state, 0, coin) => ((0, coin), Machine(state, 0, coin))
    })
  def KnobState: State[Machine, (Int, Int)] =
    State(s => s match {
      case Machine(true, can, coin) => ((can, coin), Machine(true, can, coin))
      case Machine(state, 0, coin) => ((0, coin), Machine(state, 0 , coin))
      case Machine(false, can, coin) => ((can - 1, coin), Machine(true, can-1, coin))
    })

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def stateMapper(i: Input): State[Machine, (Int, Int)] = i match {
      case Coin => CoinState
      case Turn => KnobState
    }
    chain(inputs map stateMapper)
  }
}
