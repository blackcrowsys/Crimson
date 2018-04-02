package com.blackcrowsys.crimson.common

import com.blackcrowsys.crimson.matrix.Matrix
import org.scalatest.FunSuite

class TolerenceTests extends FunSuite {

  val x = Matrix.create(Array(0.001, 1.0001, 1.002, 1.0009), 2)

  val y = Matrix.create(Array(0.001, 1.0002, 1.0025, 1.00099), 2)

  val t = new Tolerence(0.01)

  test("it should be within range of 0.1") {
    val tolerence: Tolerence = new Tolerence(0.1)

    assert(tolerence.isWithinTolerence(x, y))
  }

  test("it should not be within range of 0.00001") {
    val tolerence: Tolerence = new Tolerence(0.00001)

    assert(!tolerence.isWithinTolerence(x, y))
  }

  test("it should return false when one is null") {
    assert(!t.isWithinTolerence(x, null))
  }
  test("it should return false when both are null") {
    assert(!t.isWithinTolerence(null, null))
  }
  test("it should return false when columns are not the same") {
    assert(!t.isWithinTolerence(x, Matrix.create(Array(0.001, 1.0001, 1.002), 3)))
  }
  test("it should return false when rows are not the same") {
    assert(!t.isWithinTolerence(x, Matrix.create(Array(0.001, 1.0001, 1.002, 1.0009, 1.01, 10.2), 2)))
  }
}
