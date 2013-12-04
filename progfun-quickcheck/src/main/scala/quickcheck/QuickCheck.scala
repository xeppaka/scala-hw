package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Math._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[A]
    h <- frequency((1, empty), (7, genHeap))
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("del") = forAll {
    a: A => isEmpty(deleteMin(insert(a, empty)))
  }
  
  property("2min") = forAll {
    (a: A, b: A) => val h1 = insert(a, insert(b, empty))
    findMin(h1) == min(a, b)
  }

  property("sort") = forAll { h: H => 
    def sorted(h: H): Boolean = {
      if (isEmpty(h))
        true
      else {
        val mi = findMin(h)
        val nh = deleteMin(h)
        (isEmpty(nh) || mi <= findMin(nh)) && sorted(nh)
      } 
    }
    
    sorted(h)
  }
  
  property("meld") = forAll {
    (h1: H, h2: H) => findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }
  
  property("forum") = forAll { (h1: H, h2: H) => 
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2))
        true
      else if ((isEmpty(h1) && !isEmpty(h2)) || isEmpty(h2) && !isEmpty(h1))
        false
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    
    heapEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))  
  }
}
