package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(x) => 1
    case Branch(l: Tree[A], r: Tree[A]) => size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = {
    def loop(t: Tree[Int], currMax: Int): Int = t match {
      case Leaf(x) => currMax.max(x)
      case Branch(l: Tree[Int], r: Tree[Int]) => loop(l, currMax).max(loop(r, currMax))
    }
    loop(t, 0)
  }

  def depth[A](t: Tree[A]): Int = {
    def loop(t: Tree[A], currDepth: Int): Int = t match {
      case Leaf(x) => currDepth + 1
      case Branch(l: Tree[Int], r: Tree[Int]) => loop(l, currDepth + 1).max(loop(r, currDepth + 1))
    }
    loop(t, 0)
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l: Tree[Int], r: Tree[Int]) => Branch(map(l)(f), map(r)(f))
  }

  // TODO
  def fold[A,B](t: Tree[A], z: B)(f: (A, B) => B ): B = t match {
    case Leaf(x) => ???
    case Branch(l: Tree[Int], r: Tree[Int]) => ???
  }

}