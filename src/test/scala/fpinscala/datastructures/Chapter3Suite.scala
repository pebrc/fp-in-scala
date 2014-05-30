package fpinscala.datastructures

import fpinscala.datastructures
import datastructures.List._
import org.scalatest.FunSuite

class Chapter3Suite extends FunSuite {

  test("tail removes the head of a list") {
    assert(datastructures.List(2, 3) == tail(datastructures.List(1, 2, 3)))
  }

  test("tail on empty List is Nil") {
    assert(datastructures.Nil == tail(datastructures.Nil))
  }

  test("setHead replaces the head of a list") {
    assert(datastructures.List(4, 2, 3) == setHead(datastructures.List(1, 2, 3), 4))
  }

  test("drop 2 removes the first to elements of a list") {
    assert(datastructures.List(3) == drop(datastructures.List(1, 2, 3), 2))
  }

  test("drop 2 on a one element list gives an empty list") {
    assert(datastructures.List() == drop(datastructures.List(1), 2))
  }

  test("dropWhile removes elements of a list while predicate is true") {
    assert(datastructures.List(3) == dropWhile(datastructures.List(1, 2, 3), (e: Int) => e < 3))
  }

  test("init removes the last element from a list") {
    assert(datastructures.List(1, 2) == init(datastructures.List(1, 2, 3)))
  }

  test("length calculates the length of a given list") {
    assert(3 === length(datastructures.List(1, 2, 3)))
  }

  test("sum calculates the sum of all elements of a list") {
    assert(6 === sum2(datastructures.List(1, 2, 3)))
  }

  test("product calculates the product of all elements of list") {
    assert(24.0 === product2(datastructures.List(1, 2, 3, 4)))
  }

  test("reverse returns a new list with the original list's elements in reverse order") {
    assert(datastructures.List(3, 2, 1) === reverse(datastructures.List(1, 2, 3)))
  }

  test("append concatenates two lists") {
    assert(datastructures.List(1, 2, 3, 4) === append(datastructures.List(1, 2, 3), datastructures.List(4)))
  }

  test("flatten turns a list of lists of a into a list of a") {
    assert(datastructures.List(1, 2, 3, 4, 5) === flatten(datastructures.List(datastructures.List(1, 2), datastructures.List(3, 4), datastructures.List(5))))
  }

  test("plusOne adds one to each element of a list of Ints") {
    assert(datastructures.List(2, 3, 4) === plusOne(datastructures.List(1, 2, 3)))
  }

  test("doubleToStr turns each element of a list of double into a string") {
    assert(datastructures.List("1.0", "2.0", "3.0") === doubleToStr(datastructures.List(1, 2, 3)))
  }

  test("filter odd numbers from a list") {
    assert(datastructures.List(2, 4, 6) === filter(datastructures.List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))
  }

  test("flatMap flattens after mapping") {
    assert(datastructures.List(1, 1, 2, 2, 3, 3) == flatMap(datastructures.List(1, 2, 3))(i => datastructures.List(i, i)))
  }

  test("filter via flatMap") {
    assert(datastructures.List(2, 4, 6) === filterViaFlatMap(datastructures.List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))
  }

  test("add corresponing elements") {
    assert(datastructures.List(5, 7, 9) === zip(datastructures.List(1, 2, 3), datastructures.List(4, 5, 6))(_ + _))
  }

  test("hasSubsequence returns true when list contains another") {
    assert(hasSubsequence(datastructures.List(1, 2, 3, 4, 5), datastructures.List(2, 3, 4)))
  }

  test("hasSubsequence returns false when the other list is not a sublist") {
    assert(!hasSubsequence(datastructures.List(1, 2, 3, 4, 5), datastructures.List(6, 7, 8)));
  }

  import Tree._
  test("size calculates the number of nodes in a tree") {
    assert(3 === size(Branch(Leaf(1), Leaf(2))))
  }

  test("maximum of a tree of 1, 2, 3 is 3") {
    assert(3 === maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
  }

  test("depth of a tree of 1,2 is 1") {
    assert(1 === depth(Branch(Leaf(1), Leaf(2))))
  }

  test("folded size calculates the number of nodes in a tree via fold") {
    assert(3 === foldedSize(Branch(Leaf(1), Leaf(2))))
  }

  test("folded maximum of a tree of 1, 2, 3 is 3 calculated via fold") {
    assert(3 === foldedMaximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
  }
  test("folded depth of a tree of 1,2,3 is 2 calculated via fold") {
    assert(2 === foldedDepth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
  }

}
