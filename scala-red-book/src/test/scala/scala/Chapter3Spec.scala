import org.scalatest._

class Chapter3Spec extends WordSpec with Matchers {

  "Chapter3.tail" should {

    "return the list without the head" in {
      List.tail(List(1,2,3,4)) shouldEqual List(2,3,4)
    }

    "return the non int list without the head" in {
      List.tail(List("a","b","c")) shouldEqual List("b","c")
    }
  }

  "Chapter3.setHead" should {

    "return the list with a new head" in {
      List.setHead(List(1,2,3,4), 5) shouldEqual List(5,2,3,4)
    }

    "return a Nil list with a new head" in {
      List.setHead(List(), 5) shouldEqual List(5)
    }
  }

  "Chapter3.drop" should {

    "return last n elements of the last" in {
      List.drop(List(1,2,3,4), 2) shouldEqual List(3,4)
    }

  }

  "Chapter3.dropWhile" should {

    "return even  numbers in list" in {
      List.dropWhile(List(1,2,3,4), (a: Int) => a % 2 == 0) shouldEqual List(4,2)
    }
  }

  "Chapter3.init" should {

    "return everything except last element" in {
      List.init(List(1,2,3,4)) shouldEqual List(1,2,3)
    }
  }

  "Chapter3.length" should {

    "return the length" in {
      List.length(List(1,2,3,4)) shouldEqual 4
    }
  }

  "Chapter3.foldLeft" should {

    "return add elements in list" in {
      List.foldLeft(List(1,2),0)(_+_) shouldEqual 3
    }
  }

  "Chapter3.reverse" should {

    "reverse list" in {
      List.reverse(List(1,2,3)) shouldEqual List(3,2,1)
    }
  }

  "Chapter3.append" should {

    "append list" in {
      List.append(List(1,2,3), List(4,5,6)) shouldEqual List(1,2,3,4,5,6)
    }
  }

  "Chapter3.flatten" should {

    "flatten list" in {
      List.flatten(List(List(1,2,3), List(4,5,6))) shouldEqual List(1,2,3,4,5,6)
    }
  }


  "Chapter3.transform" should {

    "add one to each element" in {
      List.transform(List(1,2,3)) shouldEqual List(2,3,4)
    }
  }

  "Chapter3.stringFunc" should {

    "double to stirng" in {
      List.stringFunc(List(1.0,2.0,3.0)) shouldEqual List("1.0","2.0","3.0")
    }
  }

  "Chapter3.map" should {

    "map a function" in {
      List.map(List(1.0,2.0,3.0))(a => a.toString) shouldEqual List("1.0","2.0","3.0")
    }
  }
  "Chapter3.filter" should {

    "filter" in {
      List.filter(List(1,2,3,4,5))(_ % 2 == 0) shouldEqual List(2,4)
    }
  }

  "Chapter3.flatMap" should {

    "flatMap" in {
      List.flatMap(List(1,2,3))(i => List(i,i)) shouldEqual List(1,1,2,2,3,3)
    }
  }


  "Chapter3.filter2" should {

    "filter2" in {
      List.filter2(List(1,2,3))(_ % 2 == 0) shouldEqual List(2)
    }
  }

  "Chapter3.addCorresponding" should {

    "add corresponding values" in {
      List.addCorresponding(List(1,2,3), List(1,2,3)) shouldEqual List(2,4,6)
    }
  }

  "Chapter3.zipWith" should {

    "zip generic values" in {
      List.zipWith(List(1,2,3), List('a','b','c')) shouldEqual List((1,'a'),(2,'b'),(3,'c'))
    }

    "zip generic values with same type" in {
      List.zipWith(List(1,2,3), List(1,2,3)) shouldEqual List((1,1),(2,2),(3,3))
    }
  }

  "Chapter3.hasSubsequence" should {

    "return true if list has subsequence " in {
      List.hasSubsequence(List(1,2,3), List(1,2)) shouldEqual true
    }

    "return false if list hasn't got subsequence " in {
      List.hasSubsequence(List(1,2,3), List(4,5)) shouldEqual false
    }

    "return true if Nil are the same " in {
      List.hasSubsequence(Nil, Nil) shouldEqual true
    }

    "return true if list are the same " in {
      List.hasSubsequence(List(1), List(1)) shouldEqual true
    }

  }
  "Chapter3.size" should {

    "size for branch" in {
      Tree.size(Branch(Leaf("a"), Leaf("a"))) shouldBe 3
    }

    "size for branches" in {
      Tree.size(Branch(Branch(Leaf("a"), Leaf("a")), Leaf("a"))) shouldBe 5
    }

    "size for leaf" in {
      Tree.size(Leaf("a")) shouldBe 1
    }
  }


}
