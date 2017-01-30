package list

import org.scalatest.{FreeSpec, Matchers}
import lists.Lists._

class ListsTest extends FreeSpec with Matchers {

  val list = List(1, 2, 3, 4)
  val singleList = List(1)

  "should reverse" in {

    reverse(list) shouldEqual List(4,3,2,1)
    reverse(Nil) shouldEqual Nil
    reverse(singleList) shouldEqual singleList
  }

  "should flatten" in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldEqual List(1, 1, 2, 3, 5, 8)
    flatten(List(List(List(1,2), 1), 2, List(3, List(5, 8)))) shouldEqual List(1, 2, 1, 2, 3, 5, 8)
  }

  "should compress" in {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List('a, 'b, 'c, 'a, 'd, 'e)
    compress(Nil) shouldEqual Nil
    compress(singleList) shouldEqual singleList
    compress(List(1,2)) shouldEqual List(1,2)
  }

  "should pack" in {
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  }

  "should encode" in {
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }
}
