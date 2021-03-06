package scala

object Chapter2 {

  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def loop(list: List[Int]): List[Int] = {
      list.reverse match {
        case l if l.length >= n+1 => list
        case h :: t :: z => loop(list :+ (h + t))
        case h :: Nil => List(h)
        case h :: t => loop(list :+ (h + t.head))
      }
    }

    loop(List(0,1))(n)
  }


  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if(as.length >= n+2)
        if(ordered(as(n),as(n+1))) loop(n+1) else false
      else
        true
    }

    loop(0)
  }

  def curry[A,B,C](f: (A,B) => C): A => B => C = {

    a: A => b: B => f(a,b)

  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {

    a: A => f(g(a))
  }

}

