package com.blackcrowsys.crimson.common

import com.blackcrowsys.crimson.matrix.Matrix
import org.scalatest.FunSuite

class TolerenceTests extends FunSuite {

  val x = Matrix.create(Array(0.001, 1.0001, 1.002, 1.0009), 2)

  val y = Matrix.create(Array(0.001, 1.0002, 1.0025, 1.00099), 2)

  test("it should be within range of 0.1") {
    val tolerence: Tolerence = new Tolerence(0.1)

    assert(tolerence.isWithinTolerence(x, y) == true)
  }

  test("it should not be within range of 0.00001") {
    val tolerence: Tolerence = new Tolerence(0.00001)

    assert(tolerence.isWithinTolerence(x, y) == false)
  }
}
