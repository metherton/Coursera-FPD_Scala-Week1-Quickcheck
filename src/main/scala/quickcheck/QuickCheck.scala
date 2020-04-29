package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      k <- arbitrary[Int]
      l <- oneOf(const(empty), genHeap)
    } yield insert(k, l)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOf2") = forAll { (a: Int, b: Int) =>
    val i = insert(a, empty)
    val j = insert(b, empty)
    val combine = meld(i, j)
    findMin(combine) == (if (a < b) a else b)
  }

  property("insertEmpty") = forAll { a: Int =>
    val h = insert(a, empty)
    val emptyH = deleteMin(h)
    emptyH == empty
  }

  property("sorted heap") = forAll { h: H =>
    def isSorted(heap: H): Boolean = {
      if (isEmpty(heap)) true
      else {
        val a = findMin(heap)
        val remainder = deleteMin(heap)
        isEmpty(remainder) || (a <= findMin(remainder) && isSorted(deleteMin(remainder)))
      }
    }
    isSorted(h)
  }

  property("minimum of melding") = forAll { (h1: H, h2: H) =>
    val minH1 = findMin(h1)
    val minH2 = findMin(h2)
    findMin(meld(h1, h2)) == (if (minH1 < minH2) minH1 else minH2)
  }
}
