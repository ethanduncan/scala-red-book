package scala

import scala.annotation.tailrec
import scala.collection.immutable.List

object Chapter3 {

  def tail[A](l: List[A]): List[A] = {
    l  match {
      case Nil => Nil
      case _ :: t => t
    }
  }

  def setHead[A](l: List[A], newElement: A): List[A] = {
    l match {
      case Nil => Nil
      case _ :: t => newElement +: t
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def loop(l: List[A], left: Int): List[A] = {
      l match {
        case Nil => Nil
        case _ :: t if left == 1 =>  t
        case _ :: t => loop(t, left-1)
      }
    }

    loop(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @tailrec
    def loop(left: List[A], newList: List[A]): List[A] = {
      left match {
        case Nil => newList
        case h :: t if f(h) =>  loop(t, newList :+ h)
        case _ :: t => loop(t, newList)
      }
    }

    loop(l, List.empty)
  }

}
