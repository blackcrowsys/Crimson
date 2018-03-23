package com.blackcrowsys.crimson.ils

import com.blackcrowsys.crimson.common.Tolerence
import com.blackcrowsys.crimson.matrix.Matrix
import com.blackcrowsys.crimson.matrix.Matrix.Matrix
import org.scalatest.FunSuite

class GradientDescentTests extends FunSuite {

  val derivative = (x: Matrix) => x.*(2)
  val expected = Matrix.create(Array(0), 1)
  val alpha: Double = 0.2
  val tolerence: Tolerence = new Tolerence(0.000001)
  val gd = new GradientDescent();

  test("it should give the minimum of y = x^2 when given x=0") {
    val m = Matrix.create(Array(0), 1)
    val actual: Matrix = gd.iterate(derivative, m, alpha, tolerence)

    assert(tolerence.isWithinTolerence(actual, expected))
  }

  test("it should give the minimum of y = x^2 when given x=2") {
    val m = Matrix.create(Array(2), 1)

    val actual: Matrix = gd.iterate(derivative, m, alpha, tolerence)

    assert(tolerence.isWithinTolerence(actual, expected))
  }

  test("it should give the minimum of z = x^2 + y^2 when given, 2, -2") {
    val m = Matrix.create(Array(2, -2), 1)

    val actual = gd.iterate(derivative, m, alpha, tolerence)

    assert(tolerence.isWithinTolerence(actual, Matrix.create(Array(0, 0), 1)))
  }
}
