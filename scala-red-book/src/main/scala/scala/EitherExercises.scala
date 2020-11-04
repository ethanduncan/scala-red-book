package scala

import scala.{Left => _, Right => _, Either => _}

object Either {


  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = {
      this match {
        case Right(v) => Right(f(v))
        case Left(e) => Left(e)
      }
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(e) => Left(e)
        case Right(v) => f(v)
      }
    }
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(_) => b
        case Right(v) => Right(v)
      }
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      this.flatMap(x => b.map(y => f(x,y)))
    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

}
