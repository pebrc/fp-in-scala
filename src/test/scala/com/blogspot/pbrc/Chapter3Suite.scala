package com.blogspot.pbrc

import com.blogspot.pbrc.Chapter3._
import com.blogspot.pbrc.List._
import org.scalatest.FunSuite

class Chapter3Suite extends FunSuite {

  test("tail removes the head of a list") {
    assert(List(2, 3) == List.tail(List(1, 2, 3)))
  }

  test("tail on empty List is Nil") {
    assert(Nil == List.tail(Nil))
  }

  test("setHead replaces the head of a list") {
    assert(List(4, 2, 3) == List.setHead(List(1, 2, 3), 4))
  }

  test("drop 2 removes the first to elements of a list") {
    assert(List(3) == List.drop(List(1, 2, 3), 2))
  }

  test("drop 2 on a one element list gives an empty list") {
    assert(List() == List.drop(List(1), 2))
  }

  test("dropWhile removes elements of a list while predicate is true") {
    assert(List(3) == List.dropWhile(List(1, 2, 3), (e: Int) => e < 3))
  }

  test("init removes the last element from a list") {
    assert(List(1, 2) == List.init(List(1, 2, 3)))
  }

  test("length calculates the length of a given list") {
    assert(3 === List.length(List(1, 2, 3)))
  }

  test("sum calculates the sum of all elements of a list") {
    assert(6 === List.sum2(List(1, 2, 3)))
  }

  test("product calculates the product of all elements of list") {
    assert(24.0 === List.product2(List(1, 2, 3, 4)))
  }

  test("reverse returns a new list with the original list's elements in reverse order") {
    assert(List(3, 2, 1) === List.reverse(List(1, 2, 3)))
  }

  test("append concatenates two lists") {
    assert(List(1, 2, 3, 4) === List.append(List(1, 2, 3), List(4)))
  }

  test("flatten turns a list of lists of a into a list of a") {
    assert(List(1, 2, 3, 4, 5) === List.flatten(List(List(1, 2), List(3, 4), List(5))))
  }

  test("plusOne adds one to each element of a list of Ints") {
    assert(List(2, 3, 4) === List.plusOne(List(1, 2, 3)))
  }

  test("doubleToStr turns each element of a list of double into a string") {
    assert(List("1.0", "2.0", "3.0") === List.doubleToStr(List(1, 2, 3)))
  }

  test("filter odd numbers from a list") {
    assert(List(2, 4, 6) === List.filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))
  }

  test("flatMap flattens after mapping") {
    assert(List(1, 1, 2, 2, 3, 3) == List.flatMap(List(1, 2, 3))(i => List(i, i)))
  }

  test("filter via flatMap") {
    assert(List(2, 4, 6) === List.filterViaFlatMap(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))
  }

  test("add corresponing elements") {
    assert(List(5, 7, 9) === List.zip(List(1, 2, 3), List(4, 5, 6))(_ + _))
  }

  test("hasSubsequence returns true when list contains another") {
    assert(hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 4)))
  }

  test("hasSubsequence returns false when the other list is not a sublist") {
    assert(!hasSubsequence(List(1, 2, 3, 4, 5), List(6, 7, 8)));
  }
}
