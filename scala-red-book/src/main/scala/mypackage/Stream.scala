package mypackage

trait Stream[+A] {

  import Stream._

  def toList: List[A] = {
    this match {
      case Cons(h, t) => h() :: t().toList
      case _ => List()
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, _) if n == 0 => cons(h(),empty)
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ => empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }

  def headOption: Option[A] = this match
  {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => false
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def takeWhile2(f: A => Boolean): Stream[A] = {
    foldRight(this)((a, b) => if (f(a)) {
      cons(a, b)
    } else empty)
  }

  def headOption2: Option[A] = {
    foldRight[Option[A]](Option.empty)((h,t) => Some(h))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, b) => cons(f(a), b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a,b) => if (f(a)) cons(a,b) else b)
  }

  def append[B>:A](stream: => Stream[B]): Stream[B] = {
    foldRight(stream)((h,t) => cons(h,t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B]){
      (a,b) => f(a).append(b)
    }
  }

  def map2[B](f: A => B): Stream[B] = {
    unfold(this){
      case cons @ Cons(a,b) => Some((f(a()), b()))
      case _ => None
    }
  }

  def take2(n: Int): Stream[A] = {
    unfold(this, n) {
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n-1))
      case (Cons(h, _), 1) => Some((h(),(empty, 0)))
      case _ => None
    }
  }

  def takeWhile3(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h,t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val stream: Stream[A] = Stream.cons(a, stream)
    stream
  }

  def from(n: Int): Stream[Int] = {
    cons[Int](n, from(n + 1))
  }


  val fibs: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = {
      cons(a, loop(b, a+b))
    }
    loop(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  val fibs2: Stream[Int] = {
    unfold((0,1)){  case (a,b) => Some(a, (a, a + b)) }
  }

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from2(n: Int): Stream[Int] = {
    unfold(n)(n => Some(n, n + 1))
  }

  def constant2[A](a: A): Stream[A] = {
    unfold(a)(a => Some(a, a))
  }

  val ones2 = unfold(1){f => Some(1, 1)}

}