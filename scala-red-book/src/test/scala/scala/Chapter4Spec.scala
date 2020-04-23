package scala

import org.scalatest.{Matchers, WordSpec}

import scala.{Option => _, Either => _, _}


class Chapter4Spec extends WordSpec with Matchers {

  "Chapter4 Option.map" should {
    "map a function over an option" in {
      Some(1).map(_ + 1) shouldEqual Some(2)
    }

    "map2" in {
      Option.map2(Some(2),Some(3))(_ + _) shouldEqual Some(5)
    }
  }

}
