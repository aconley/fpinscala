package fpinscala.gettingstarted

// A comment!
/* Another comment */
/** A documentation comment */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))

  // A definition of factorial, using a local, tail recursive function
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  // Another implementation of `factorial`, this time with a `while` loop
  def factorial2(n: Int): Int = {
    var acc = 1
    var i = n
    while (i > 0) { acc *= i; i -= 1 }
    acc
  }

  // Exercise 1: Write a function to compute the nth fibonacci number
  //  A few solutions presented

  // Probably the one they had in mind
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibInner(idx: Int, prev: Int, curr: Int): Int = 
      if (idx <= 0) prev else fibInner(idx - 1, curr, prev + curr)
    fibInner(n, 0 , 1)
  }

  // Stream solution, requires 2.11
  //def fib2(n: Int): Int = {
  //  require(n >= 1, "Invalid n")
  //  val str: Stream[Int] = 0 #:: 1 #:: str.zip(str.tail).map(x => x._1 + x._2)
  //  str(n-1)
  //}
  // Stream solution works in 2.8
  def fib3(n:Int): Int = {
    def fibFrom(a: Int, b: Int): Stream[Int] = a #:: fibFrom(b, a + b)
    fibFrom(0, 1)(n-1)
  }

  // Scan left stream solution, requires 2.11
  //def fib4(n: Int): Int = {
  //  require(n >= 1, "Invalid n")
  //  val str: Stream[Int] = 0 #:: str.scanLeft(1)(_ + _)
  //  str(n-1)
  //}

  // This definition and `formatAbs` are very similar..
  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  // We can generalize `formatAbs` and `formatFactorial` to
  // accept a _function_ as a parameter
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }
}

object FormatAbsAndFactorial {

  import MyModule._

  // Now we can use our general `formatResult` function
  // with both `abs` and `factorial`
  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }
}

// Functions get passed around so often in FP that it's
// convenient to have syntax for constructing a function
// *without* having to give it a name
object AnonymousFunctions {
  import MyModule._

  // Some examples of anonymous functions:
  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("increment", 7, (x: Int) => x + 1))
    println(formatResult("increment2", 7, (x) => x + 1))
    println(formatResult("increment3", 7, x => x + 1))
    println(formatResult("increment4", 7, _ + 1))
    println(formatResult("increment5", 7, x => { val r = x + 1; r }))
  }
}

object MonomorphicBinarySearch {

  // First, a binary search implementation, specialized to `Double`,
  // another primitive type in Scala, representing 64-bit floating
  // point numbers
  // Ideally, we could generalize this to work for any `Array` type,
  // so long as we have some way of comparing elements of the `Array`
  def binarySearch(ds: Array[Double], key: Double): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val d = ds(mid2) // We index into an array using the same
                         // syntax as function application
        if (d == key) mid2
        else if (d > key) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, ds.length - 1)
  }

}

object PolymorphicFunctions {

  // Here's a polymorphic version of `binarySearch`, parameterized on
  // a function for testing whether an `A` is greater than another `A`.
  def binarySearch[A](as: Array[A], key: A, gt: (A,A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key,a)) mid2
        else if (greater) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

  // Exercise 2: Implement a polymorphic function to check whether
  // an `Array[A]` is sorted
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
<<<<<<< HEAD
    val len = as.length
    @annotation.tailrec
    def isSortedInner(idx: Int, prev: A): Boolean = {
      if (idx == len) true
      else if (gt(as(idx), prev)) isSortedInner(idx + 1, as(idx))
      else false
    }

    if (len == 0) true // well, sorta defined
    else isSortedInner(1, as(0))
=======
    @annotation.tailrec
    def inner(i: Int, prev: A, lenas: Int): Boolean = {
      if (i == lenas) true // Reached the end
      else if (gt(as(i), prev)) inner(i + 1, as(i), lenas)
      else false
    }
    inner(1, as(0), as.length)
>>>>>>> db36077346127e83724933871b06d7d554b57ffa
  }

  // Polymorphic functions are often so constrained by their type
  // that they only have one implementation! Here's an example:

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  // Exercise 3: Implement `curry`.

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => (b => f(a, b))

  // NB: The `Function2` trait has a `curried` method already

  // Exercise 4: Implement `uncurry`
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  /*
  NB: There is a method on the `Function` object in the standard library,
  `Function.uncurried` that you can use for uncurrying.

  Note that we can go back and forth between the two forms. We can curry
  and uncurry and the two forms are in some sense "the same". In FP jargon,
  we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
  a term we inherit from category theory.
  */

  // Exercise 5: Implement `compose`

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
