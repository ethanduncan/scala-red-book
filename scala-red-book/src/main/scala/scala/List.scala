import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => t
    }
  }

  def setHead[A](l: List[A], newHead: A): List[A] = {
    l match {
      case Nil => Cons(newHead, Nil)
      case Cons(_, t) => Cons(newHead, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def loop(l: List[A], left: Int): List[A] = {
      l match {
        case Nil => Nil
        case Cons(_,t) if left <= 1 => t
        case Cons(_,t) => loop(t, left-1)
      }
    }

    loop(l, n)

  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @tailrec
    def loop(left: List[A], newList: List[A]): List[A] = {
      left match {
        case Nil => newList
        case Cons(h,t) if f(h) => loop(t, Cons(h,newList))
        case Cons(_,t) => loop(t, newList)
      }
    }
    loop(l, Nil)
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((a,total) => total + 1)
  }

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h,t) => foldLeft(t,f(z,h))(f)
    }
  }

  def sum2(l: List[Int]): Int = {
    foldLeft(l,0)(_ + _)
  }

  def product2(l: List[Int]): Int = {
    foldLeft(l,1)(_ * _)
  }

  def length2[A](l: List[A]): Int = {
    foldLeft(l,0)((accum,_) => accum + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l,List[A]())((computed, nextElement) => Cons(nextElement, computed))
  }

  def foldRightViaLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as),z)((b,a) => f(a,b))
  }

  def append[A](left: List[A], right: List[A]): List[A] = {
    foldRight(left, right)((a,b) => Cons(a,b))
  }

//  def append2[A](left: List[A], right: List[A]): List[A] = {
//    foldLeft(reverse(right), reverse(left))((b, a) => Cons(b,a))
//  }

  def flatten[A](list: List[List[A]]): List[A] = {
//    foldRight(list, List[A]())((a,b) => append(a,b))
    foldLeft(list, List[A]())((a, b) => append(a,b))
  }

  def transform(list: List[Int]): List[Int] = {
    foldRight(list, List[Int]())((h,t) => Cons(h+1, t))
  }

  def stringFunc(list: List[Double]): List[String] = {
    foldRight(list, List[String]())((h,t) => Cons(h.toString, t))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, List[B]())((h, t) => Cons(f(h), t))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List[A]())((h ,t) => if(f(h)) Cons(h, t) else t)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    flatten(map(as)(f))
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def addCorresponding(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addCorresponding(t1, t2))
  }
//
//  def addCorresponding[A](a: List[A], b: List[A]): List[A] = (a, b) match {
//    case (_, Nil) => Nil
//    case (Nil, _) => Nil
//    case (Cons(h1, t1), Cons(h2, t2)) => {
//      val test: A = h1 + h2
//      Cons(h1 + h2, addCorresponding(t1, t2))
//    }
//  }

  def zipWith[A,B](a: List[A], b: List[B]): List[(A,B)] = (a,b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1,h2), zipWith(t1,t2))
  }

  def beginsWith[A](list: List[A], begin: List[A]): Boolean = {
    (list,begin) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => beginsWith(t1, t2)
      case _ => false
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case sup if sup == sub => true
      case sup if beginsWith(sup, sub) => true
      case Cons(_, t) if hasSubsequence(t, sub) => true
      case _ => false
    }
  }
}