package fpinscala.monoids

// Note we only use the Nonblocking versions
import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  // Flips the order on the operation
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a: A, b: A): A = m.op(b, a)
    val zero = m.zero
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(i: Int, j: Int) = i + j
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(i: Int, j: Int) = i * j
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a || b
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a && b
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(o1: Option[A], o2: Option[A]): Option[A] = o1 orElse o2
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    // Note that this could also be f andThen g
    def op(f: A => A, g: A => A): A => A = f compose g
    def zero = a => a
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(o1: (A, B), o2: (A, B)) = (A.op(o1._1, o2._1), B.op(o1._2, o2._2))
      val zero = (A.zero, B.zero)
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(f: A => B, g: A => B) = a => B.op(f(a), g(a))
      val zero: A => B = a => B.zero
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero){ (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))}
      val zero = Map[K, V]()
    }

  import fpinscala.testing._
  import Prop._

  // Test associativity
  def monoidLawsAssociative[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val tvals = for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)
    def assoc(p: (A, A, A)): Boolean = p match {
      case (x, y, z) => m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)
    }
    forAll(tvals)(assoc)
  }
  def monoidLawsIdentity[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen)(a => (m.op(a, m.zero)) == a && m.op(m.zero, a) == a)

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Note this could be m.op(f(a), b) instead...
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  // It would be better to have this cut off at some small size
  // rather than recurse all the way down, but this is, after all,
  // only for pedagogical purposes
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length == 0)
      m.zero
    else if (as.length == 1)
      f(as(0))
    else {
      val (left, right) = as.splitAt(as.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }

  // Lift a Monoid into the parallel context
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a: Par[A], b: Par[A]) = a.map2(b)(m.op)
    def zero = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap(x => foldMapV(x, par(m))(y => Par.lazyUnit(y)))

  def ordered(ints: IndexedSeq[Int]): Boolean =
    sys.error("todo")

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a: WC, b: WC) = (a, b) match {
      case (Stub(x), Stub(y)) => Stub(x + y)
      case (Stub(x), Part(l, w, r)) => Part(x + l, w, r)
      case (Part(l, w, r), Stub(x)) => Part(l, w, r + x)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        if ((r1 + l2).isEmpty)
          Part(l1, w1 + w2, r2)
        else // New word formed from r1 and l2 joining
          Part(l1, w1 + w2 + 1, r2)
    }
    val zero = Stub("")
  }

  // Smart constructor for chars that skips whitespace
  def wc(c: Char): WC =
    if (c.isWhitespace)
      Part("", 0, "")
    else
      Stub(c.toString)

  // This is spectacularly inefficient
  def count(s: String): Int = {
    // At the end we will need to consider any remaining strings on the end
    //  to be finished words
    def finishWord(s: String): Int = if (s.isEmpty) 0 else 1
    // Combine all the stubs
    val w = foldMapV(s.toIndexedSeq, wcMonoid)(wc)
    w match {
      case Stub(x) => finishWord(x)
      case Part(l, w, r) => finishWord(l) + w + finishWord(r)
    }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)  // Doesn't like Nil...
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(a) => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(l)(foldLeft(r)(z)(f))(f)
    }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(r)(foldRight(l)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(a) => f(a)
    }
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case None => z  // Only value of B available...
      case Some(a) => f(z, a)
    }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as match {
      case None => z
      case Some(a) => f(a, z)
    }
}

