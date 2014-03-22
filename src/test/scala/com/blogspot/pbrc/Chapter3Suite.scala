package com.blogspot.pbrc

import com.blogspot.pbrc.Chapter1._
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
    assert(List(3) == List.dropWhile(List(1,2,3), (e:Int) => e < 3))
  }

  test("init removes the last element from a list") {
    assert(List(1,2) == List.init(List(1,2,3)))
  }

  test("length calculates the length of a given list") {
    assert(3 === List.length(List(1,2,3)))
  }

  test("sum calculates the sum of all elements of a list") {
    assert(6 === List.sum2(List(1,2,3)))
  }

  test("product calculates the product of all elements of list") {
    assert(24.0 === List.product2(List(1,2,3,4)))
  }

}
