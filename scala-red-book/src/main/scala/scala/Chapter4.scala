//package scala
//
//import scala.{Option => _, Either => _, _}
//
//object Chapter4 {
//
//  sealed trait Option[+A]{
//
//    def map[B](f: A => B): Option[B] = {
//      this match {
//        case None => None
//        case Some(a: A) => Some(f(a))
//      }
//    }
//
//    def flatMap[B](f: A => Option[B]): Option[B] = {
//      this match {
//        case None => None
//        case Some(a) => f(a)
//      }
//    }
//
//    def getOrElse[B >: A](default: => B): B = {
//      this match {
//        case None => default
//        case Some(a) => a
//      }
//    }
//
//    def orElse[B >: A](ob: => Option[B]): Option[B] = {
//      this match {
//        case None => ob
//        case _ => this
//      }
//    }
//
//    def filter(f: A => Boolean): Option[A] = {
//      this match {
//        case Some(a) if f(a) => this
//        case _ => None
//      }
//    }
//
//
//  }
//
//
//  def variance(xs: Seq[Double]): Option[Double] = {
//    def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum/xs.length)
//    mean(xs).flatMap(mn => mean(xs.map(x => math.pow(x - mn, 2.0))))
//  }
//
//  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
//
//  //    def lift2[A,B,C](f: (A,B) => C): (Option[A], Option[B]) => Option[C] =
//
//  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
//    a.flatMap(x => b.map(y => f(x,y)))
//  }
//
//  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
//    a.foldRight[Option[List[A]]](Some(List.empty))((h,t) => map2(h,t)((h1, t1) => t1.appended(h1)))
//  }
//
////  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
////     a match {
////       case Nil => None
//  //       case List(a) => map2(Some(a), f(a))((a1, b) => b)
////     }
////  }
//
//  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
//    a.foldRight[Option[List[B]]](Some(List.empty))((h,t) => map2(f(h), t)((h1, t1) => t1.appended(h1)))
//  }
//
//  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
//    traverse(a)(a => a)
//  }
//
//  case class Some[+A](get: A) extends Option[A]
//  case object None extends Option[Nothing]
//
//
//}