package fpinscala.datastructures

// Note that empty leafs are not supported!
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
   * Return the size of a tree
   * @param t The tree
   * @tparam A The type of elements in the tree
   * @return The number of elements in the tree
   */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  /**
   * Get the number of values in a tree
   * @param t The tree
   * @tparam A The type of elements in the tree
   * @return The number of leaf nodes in the tree
   */
  def nLeaves[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => nLeaves(l) + nLeaves(r)
  }

  /**
   * Get the maximum element in a tree
   * @param t The tree to search
   * @param ord The ordering of elements
   * @tparam A The type of elements in tree
   * @return The largest element using the specified ordering
   */
  def maximum[A](t: Tree[A])(implicit ord: Ordering[A]): A = t match {
    case Leaf(x) => x
    case Branch(l, r) => ord.max(maximum(l), maximum(r))
  }

  /**
   * The maximum depth of the tree
   * @param t The tree to examine
   * @tparam A The type of elements of the tree
   * @return The depth
   */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(x) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  /**
   * Map a function over a tree
   * @param t The tree to map over
   * @param f The function to apply to each element
   * @tparam A The type of elements in the initial tree
   * @tparam B The type of elements in the output tree
   * @return A new tree formed by mapping the function over the input
   */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
   * Fold a pair of functions over a tree
   * @param t The tree
   * @param f The function to apply to each element
   * @param g The function to combine each element after f is applied to it
   * @tparam A The type of element in the input tree
   * @tparam B The type of output
   * @return The fold using f on each Leaf and g to join Leaf values
   */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def nLeavesFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _)

  def maximumFold[A](t: Tree[A])(implicit ord: Ordering[A]): A =
    fold(t)(x => x)(ord.max(_, _))

  def depthFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((x, y) => 1 + (x max y))

  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
}