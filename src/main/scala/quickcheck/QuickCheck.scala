package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  // Empty generator
  val emptyGen = Gen.const(empty)

  var counter = 0

  lazy val uniqueGen = Gen.resultOf { _: A =>
    counter += 1
    if (counter % 2 == 0) counter else -counter
  }

  // Generators are for generating random values for types
  // For example here i am inserting arbitrary values into a unknown heap type H
  // On runtime this will be `H` => heap of int and `A` => Int
  // there are many generators functions like oneOf, choose etc
  // check ScalaCheck docs.
  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    m <- Gen.frequency((3, emptyGen), (7, genHeap))
  } yield insert(k, m)

  lazy val uniqueGenHeap: Gen[H] = for {
    k <- uniqueGen
    m <- Gen.frequency((2, emptyGen), (8, uniqueGenHeap))
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // Properties are for testing. You can use generators data to check for a
  // property and see if its valid or if no generator is mentioned it
  property("finding min") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // Or if you don't define generator and types are one for which ScalaCheck
  // can generate data itself then it will work. For example here on runtime A
  // will be Int so it will generate random int.
  property("adding two elem in empty and finding min") = forAll { (a: A, b: A) =>
    // You can also define filters inside properties like a > b
    (a > b) ==> {
      val h1 = insert(a, empty)
      val h2 = insert(b, h1)
      findMin(h2) == b
    }
  }

  property("add and delete from empty should be empty") = forAll { (a: A) =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  property("consecutively finding and deleting should give back sorted list") = forAll(uniqueGenHeap) { (h: H) =>
    (!isEmpty(h)) ==> {
      def takeMin(h: H, list: List[A]): List[A] = {
        if (isEmpty(h)) list
        else takeMin(deleteMin(h), findMin(h) :: list)
      }
      val list = takeMin(h, List())
      if (list.distinct.size < list.size) false
      else if (list.size <= 1) true
      else list.sliding(2).forall { case List(x, y) => ord.gteq(x, y) }
    }
  }


  property("minimum of combination of two heaps is smaller of individual minimums") =
    forAll { (h1: H, h2: H) =>
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      val min = if (m1 > m2) m2 else m1
      min == findMin(meld(h1, h2))
    }

  property("consecutive calls to findMin should give back same results") =
    forAll { (h: H) =>
      (!isEmpty(h)) ==> {
        findMin(h) == findMin(h)
      }
    }

  property("adding smaller elem in heap should give back that elem") = forAll { (h: H) =>
    (!isEmpty(h) && findMin(h) != Integer.MIN_VALUE) ==> {
      val oldMin = findMin(h)
      val newMin = oldMin - 1
      val newH = insert(newMin, h)
      val foundMin = findMin(newH)
      foundMin == newMin
    }
  }
}
