package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Will stack overflow on long lists
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  def take(n: Int): Stream[A] = {
    require(n >= 0, "Negative n in take")
    this match {
      case Cons(x, xs) if n > 1 => cons(x(), xs() take (n - 1))
      case Cons(x, _) if n == 1 => cons(x(), empty)
      case Empty => empty
    }
  }

  def takeUnfold(n: Int): Stream[A] = {
    def unf(s: (Stream[A], Int)): Option[(A, (Stream[A], Int))] = s match {
      case (Empty, _) => None
      case (Cons(x, _), 1) => Some((x(), (empty, 0)))
      case (Cons(x, xs), m) => Some((x(), (xs(), n-1)))
    }
    unfold((this, n))(unf)
  }

  def drop(n: Int): Stream[A] = {
    require(n >= 0, "Negative n in drop")
    this match {
      case Cons(x, xs) if n > 0 => xs() drop (n-1)
      case _ => this
    }
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(x, xs) if f(x()) => cons(x(), xs() takeWhile f)
    case _ => empty
  }

  def takeWhileUnfold(f: A => Boolean): Stream[A] = {
    def unf(s: Stream[A]): Option[(A, Stream[A])] = s match {
      case Cons(x, xs) if f(x()) => Some(x(), xs())
      case _ => None
    }
    unfold(this)(unf)
  }

  // Note that foldRight does the x()ing for us
  def takeWhileFold(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, xs) => if (f(x)) cons(x, xs) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((x, xs) => cons(f(x), xs))

  def mapUnfold[B](f: A => B): Stream[B] = {
    def unf(s: Stream[A]) = s match {
      case Cons(x, xs) => Some(f(x()), xs())
      case _ => None
    }
    unfold(this)(unf)
  }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, xs) => if (f(x)) cons(x, xs) else xs)

  // Oddly enough, the scala compiler gets pissy if we don't
  //  do the superclass (B >: A) thing because that puts a covariant
  //  thing (A) into a contravariant position.  Ah, the joys of fighting
  //  the scala compiler
  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((x, xs) => cons(x, xs))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((x, xs) => f(x) append xs)

  // Can be done with foldRight -- but why would you want to?
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(x, _) => Some(x())
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = {
    def unf(ss: (Stream[A], Stream[B])): Option[(C, (Stream[A], Stream[B]))] =
      (ss._1, ss._2) match {
        case (Empty, _) => None
        case (_, Empty) => None
        case (Cons(x, xs), Cons(y, ys)) => Some(f(x(), y()), (xs(), ys()))
    }
    unfold((this, s))(unf)
  }

  def zip[B](s: Stream[B]): Stream[(A, B)] =
    zipWith(s)((_,_))

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
    def unf(ss: (Stream[A], Stream[B])): Option[((Option[A], Option[B]), (Stream[A], Stream[B]))] =
      (ss._1, ss._2) match {
        case (Cons(x, xs), Cons(y, ys)) => Some((Some(x()), Some(y())), (xs(), ys()))
        case (Empty, Empty) => None
        case (Empty, Cons(x, xs)) => Some((None, Some(x())), (Empty, xs()))
        case (Cons(x, xs), Empty) => Some((Some(x()), None), (xs(), Empty))
    }
    unfold((this, s))(unf)
  }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s) takeWhile(x => ! x._2.isEmpty) forAll(t => t._1 == t._2)

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append (Stream(empty))

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // The canonical way (used in the scala docs) involves zip,
  // but we haven't defined that yet
  val fibs: Stream[Int] = {
    def fib_inner(x: Int, y:Int): Stream[Int] = cons(x, fib_inner(y, x+ y))
    fib_inner(0, 1)
  }

  // I guess the idea here is that f returns None if the stream is done
  // and otherwise gives you a tuple of the stream value and new state variable
  // Takes an initial state z and a function which produces the next
  //  value and the next state
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((stream_value, state)) => cons(stream_value, unfold(state)(f))
    }

  def fromUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n+1)))

  val fibsUnfold: Stream[Int] =
    unfold((0, 1))(tup => Some((tup._1, (tup._2, tup._1 + tup._2))))
}