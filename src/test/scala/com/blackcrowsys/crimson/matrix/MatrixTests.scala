package com.blackcrowsys.crimson.matrix

import org.scalatest.FunSuite

class MatrixTests extends FunSuite {

  val x = Matrix.create(Array(12.5, 4.8, 3.4, 2), 2)
  val y = Matrix.create(Array(2, 4, 6, 8), 2)
  val z = Matrix.create(Array(2, 5), 1)
  val expected = Matrix.create(Array(14.5, 8.8, 9.4, 10), 2)

  test("it gets the right values by index") {
    val e1 = x.get(1, 1)
    assert(e1 == 12.5)

    val e2 = x.get(1, 2)
    assert(e2 == 4.8)

    val e3 = x.get(2, 1)
    assert(e3 == 3.4)

    val e4 = x.get(2, 2)
    assert(e4 == 2)
  }

  assertThrows[IllegalArgumentException] {
    val e = x.get(1, 3)
  }

  assertThrows[IllegalArgumentException] {
    val e = x.get(3, 1)
  }

  assertThrows[IllegalArgumentException] {
    val y = Matrix.create(Array(12.2, 4, 7), 2)
  }

  assertThrows[IllegalArgumentException] {
    val y = Matrix.create(Array(12.1), 2)
  }

  test("two matrix are same") {
    assert(x.equals(x))
  }

  test("two matrix are not the same") {
    assert(x.equals(y) == false)
  }

  test("it can add two matrix together") {
    val actual = x + y
    assert(actual.equals(expected))
  }

  assertThrows[IllegalArgumentException] {
    x + z
  }

  test("multiplication with uneven but matched matrices") {
    val actual = x * z

    assert(Matrix.create(Array(49, 16.8), 1).equals(actual))
  }

  test("scalar multiplication of a matrix") {
    val actual = x * 2

    assert(Matrix.create(Array(25, 9.6, 6.8, 4), 2).equals(actual))
  }

}
