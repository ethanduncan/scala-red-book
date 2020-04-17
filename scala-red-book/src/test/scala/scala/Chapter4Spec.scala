package scala

import org.scalatest.{Matchers, WordSpec}

class Chapter4Spec extends WordSpec with Matchers {

  "Chapter4 Option.map" should {
    "map a function over an option" in {
      Chapter4.Some(1).map(_ + 1) shouldEqual Chapter4.Some(2)
    }
  }

}
