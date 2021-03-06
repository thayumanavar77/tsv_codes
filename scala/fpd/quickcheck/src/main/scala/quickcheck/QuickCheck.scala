package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    y <- oneOf(const(empty), genHeap)
  } yield insert(v, y)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("emptyHeap") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("minm") = forAll { (x1: Int, x2: Int) =>
    val h = insert(x2, insert(x1, empty))
    findMin(h) == Math.min(x2, x1)
  }

  property("checkElem") = forAll { (h: H, x: Int) =>
    def elemExists(h: H, elem: Int): Boolean =
      if (isEmpty(h)) false
      else if (x == findMin(h)) true else elemExists(deleteMin(h), elem)
    elemExists(insert(x,h),x)
  }

  property("minn") = forAll { (x1: Int, x2: Int) =>
    val h = insert(x2, insert(x1,empty))
    isEmpty(deleteMin(deleteMin(h)))
  }


  property("meld1") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("sorted") = forAll { (h: H) =>
    def sortedList(h: H): List[Int] = if (isEmpty(h)) List() else findMin(h) :: sortedList(deleteMin(h))
    def isSorted(xs: List[Int]): Boolean =  xs match {
      case Nil => true
      case x :: Nil => true
      case x :: y :: tail => if (x > y ) false else isSorted(tail)
    }
    isSorted(sortedList(h))
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
}
