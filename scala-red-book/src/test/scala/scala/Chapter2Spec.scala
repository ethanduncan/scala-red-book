package scala

import org.scalatest._

class Chapter2Spec extends WordSpec with Matchers {

  "Chapter2.Fibonacci" must {

    "return 34" in {
      Chapter2.fibonacci(10) shouldEqual 34
    }

    "return -34" in {
      Chapter2.fibonacci(-10) shouldEqual 34
    }
  }
}
