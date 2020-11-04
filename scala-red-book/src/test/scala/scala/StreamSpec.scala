package scala

import org.scalatest.{Matchers, WordSpec}

class StreamSpec extends WordSpec with Matchers {


  "Stream.forAll" should {
    import mypackage._

    "checks elements in the Stream match a predicate" in {
      val stream = Cons[Int](
        () => 2,
        () =>
          Cons[Int](
            () => 4,
            () =>
              Cons[Int](
                () => 7,
                () =>
                  Cons[Int](
                    (() => { throw new Exception("no") }): () => Int,
                    () => mypackage.Stream.empty[Int]
                  )
              )
          )
      )

      stream.forAll(x => x % 2 == 0) == false
    }
  }

}
