package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.

  /**
   * List helper for creating Lists
   * @param as Values
   * @tparam A Type of elements
   * @return A new List[A] containing the specified elements
   */
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
   * Sum a list of integers
   * @param ints List of integers to sum
   * @return The sum of the elements of ints
   */
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  /**
   * Form the product of a list of doubles
   * @param ds The list of doubles
   * @return The product of the elements of ds
   */
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0  // This isn't well defined, really, but is convenient so Nil can be the zero of the list Monoid
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y  // Matches this one with x = 1, y = 2
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  /**
   * Append to Lists together to form a new List
   * @param a1 First list
   * @param a2 Second list
   * @tparam A Type of both lists
   * @return A new list formed from appending a2 to the
   *         end of a1
   */
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2 // a1 is exhausted
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  /**
   * The infamous foldRight operation, not tail recursive
   * @param as List to fold
   * @param z Zero
   * @param f Binary combination function
   * @tparam A Type of input list
   * @tparam B Type of zero and returned list
   * @return The value from folding
   */
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /**
   * An alternative implementation of sum using fold
   * @param ns List to sum
   * @return The sum of the elements in ns
   */
  def sumRight(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  /**
   * An alternative implementation of product using fold
   * @param ns List to form the product of
   * @return The product of the elements in ns
   */
  def productRight(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  // Chose to return a new list if passed an empty one
  // rather than throw an error
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  /**
   * Drop the first n elements of a list
   * @param l The input list
   * @param n The number of elements to drop
   * @tparam A The type of the list
   * @return A copy of the list with the first n elements removed
   */
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil // Don't throw exception
      case Cons(_, t) => drop(t, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }

  def head[A](l: List[A]): A = l match {
    case Nil => sys.error("Head of empty list")
    case Cons(x, _) => x
  }

  /**
   * Gets the tail of a list
   * @param l Input list
   * @tparam A Type of list
   * @return All but the first element of l as a list
   */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Tail of empty List")
    case Cons(_, t) => t
  }

  /**
   * Gets everything but the last element of a list
   * @param l The list
   * @tparam A The type of elements in the list
   * @return The list with the last element removed
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  /**
   * Get the length of a list
   * @param l The list
   * @tparam A The type of elements in the list
   * @return The length of the list
   */
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, len) => len + 1)

  /**
   * The infamous foldLeft operation, which is tail recursive
   * @param l The list to fold
   * @param z The zero of the fold
   * @param f The binary operation
   * @tparam A Type of input list
   * @tparam B Type of zero and output list
   * @return The result of the fold
   */
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // foldLeft implementations of the same things
  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def productLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((len, _) => len + 1)
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((z, x) => Cons(x, z))

  // foldRight in terms of foldLeft -- the opposite is not that interesting
  // because it makes a stack-unsafe version of foldLeft
  //  This is stack safe... but reverse isn't all that cheap
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldLeft(reverse(l), z)((b, a)=> f(b, a))

  // Recall that fold right is like replacing Nil by z and cons by f
  //  so we can replace Nil by r and cons by cons to get an append.
  def appendRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  // This works by similar logic
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  // Ignore the problems of stack overflow
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, xs) => Cons(f(x), xs))

  def add1(l: List[Int]): List[Int] = map(l)(_ + 1)
  def doubleToString(l: List[Double]): List[String] =
    map(l)(_.toString)

  // Again, ignore stack overflow
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // This is rather silly...
  def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else Nil)

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case(_, Nil) => Nil
    case(Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def addPairs(a: List[Int], b: List[Int]): List[Int] =
    zipWith(a, b)(_ + _)

  // The subsequence checker is hella inefficient
  def startsWith[A](pre: List[A], l: List[A]): Boolean = (pre, l) match {
    case (Nil, _) => true  // pre is exhausted successfully
    case (Cons(p, ps), Cons(x, xs)) => if (p != x) false else startsWith(ps, xs)
    case _ => false
  }

  // This works by trying the sequence starting at every position;
  // it might be more efficient to figure out the length and decrement
  // a counter, but that depends if the subsequence happens to be near
  // the front.
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case Cons(x, xs) => if (startsWith(sub, sup)) true else hasSubsequence(xs, sub)
  }
}