package scala

import org.scalatest._

class Chapter3Spec extends WordSpec with Matchers {

  "Chapter3.tail" should {

    "return the list without the head" in {
      Chapter3.tail(List(1,2,3,4)) shouldEqual List(2,3,4)
    }

    "return the non int list without the head" in {
      Chapter3.tail(List("a","b","c")) shouldEqual List("b","c")
    }
  }

  "Chapter3.setHead" should {

    "return the list with a new head" in {
      Chapter3.setHead(List(1,2,3,4), 5) shouldEqual List(5,2,3,4)
    }
  }

  "Chapter3.drop" should {

    "return last n elements of the last" in {
      Chapter3.drop(List(1,2,3,4), 2) shouldEqual List(3,4)
    }
  }

  "Chapter3.dropWhile" should {

    "return even  numbers in list" in {
      Chapter3.dropWhile(List(1,2,3,4), (a: Int) => a % 2 == 0) shouldEqual List(2,4)
    }
  }
}
