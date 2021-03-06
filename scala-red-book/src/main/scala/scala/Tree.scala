package scala

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Branch(left, right) => 1 + size(left) + size(right)
      case Leaf(_) => 1
    }
  }

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(int) => int
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + depth(left) max depth(right)
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }
}
