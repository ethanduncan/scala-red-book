package scala

import org.scalatest._

class Chapter2Spec extends WordSpec with Matchers {

  "Chapter2.Fibonacci" should {

    "return 55" in {
      Chapter2.fibonacci(10) shouldEqual 55
    }

    "return 0" in {
      Chapter2.fibonacci(0) shouldEqual 0
    }

    "return 1" in {
      Chapter2.fibonacci(1) shouldEqual 1
    }

    "return 1 when n=2" in {
      Chapter2.fibonacci(2) shouldEqual 1
    }
  }

  "Chapter2.isSorted" should {

    "return true" in {
      Chapter2.isSorted[Int](Array(0,1,2,3), (a,b) => a < b) shouldEqual true
    }

    "return false" in {
      Chapter2.isSorted[Int](Array(4,1,3,2), (a,b) => a < b) shouldEqual false
    }

    "return true when given empty array" in {
      Chapter2.isSorted[Int](Array.empty, (a,b) => a < b) shouldEqual true
    }

    "return true when given array of length 1" in {
      Chapter2.isSorted[Int](Array(0), (a,b) => a < b) shouldEqual true
    }
  }
}
