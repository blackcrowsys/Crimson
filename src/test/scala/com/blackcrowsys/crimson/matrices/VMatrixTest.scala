package com.blackcrowsys.crimson.matrices

import com.blackcrowsys.crimson.errors.CrimsonError
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VMatrixTest extends AnyFlatSpec with Matchers {

  val row1 = Array(1, 2, 3): Array[Double]
  val row2 = Array(4, 5, 6): Array[Double]
  val contents = Array(row1, row2)


  it should "create a matrix from an array" in {
    val result = VMatrix.fromArray(contents)

    result.rows shouldBe 2
    result.columns shouldBe 3
    result.getRow(1) shouldBe row1.toVector
    result.getColumn(2) shouldBe Array(2, 5).toVector
  }

  it should "not create a matrix from different sized array" in {
    val row1 = Array(1, 2, 3): Array[Double]
    val row2 = Array(4, 5, 6, 7): Array[Double]
    val matrix = Array(row1, row2)

    an[CrimsonError] should be thrownBy VMatrix.fromArray(matrix)
  }

  it should "apply a given function to each of the values in the matrix" in {
    val m = VMatrix.fromArray(contents)
    val f = (a: Double) => Math.pow(a, 2)

    val result = m.applyToEach(f)
    result shouldBe VMatrix.fromArray(Array(Array(1, 4, 9), Array(16, 25, 36)))
  }

  it should "give a sum of the difference squared" in {
    val m = VMatrix.fromArray(contents)

    val result = m.sumOfSquaredDifference(3)

    result shouldBe 19
  }

  it should "transpose a matrix" in {
    val m = VMatrix.fromArray(contents)

    val result = m.transpose

    result shouldBe VMatrix.fromArray(Array(Array(1, 4), Array(2, 5), Array(3, 6)))
    result.transpose shouldBe m
  }

  it should "multiply by scalar" in {
    val m = VMatrix.fromArray(contents)

    val result = m.scalar(2)

    result shouldBe VMatrix.fromArray(Array(Array(2, 4, 6), Array(8, 10, 12)))
  }

  it should "carry out a dot product on two matrix" in {
    val m = VMatrix.fromArray(contents)
    val n = m.transpose

    val result = m.dot(n)

    result shouldBe VMatrix.fromArray(Array(Array(14, 32), Array(32, 77)))
  }

  it should "get sums of rows" in {
    val m = VMatrix.fromArray(contents)

    m.sumOfRow(1) shouldBe 6
    m.sumOfRow(2) shouldBe 15
  }

  it should "get sums of columns" in {
    val m = VMatrix.fromArray(contents)

    m.sumOfColumn(1) shouldBe 5
    m.sumOfColumn(2) shouldBe 7
    m.sumOfColumn(3) shouldBe 9
  }

  it should "add two matrices together" in {
    val m = VMatrix.fromArray(contents)
    val n = VMatrix.fromArray(Array(Array(7, 8, 9), Array(10, 11, 12)))

    m.sum(n) shouldBe VMatrix.fromArray(Array(Array(8, 10, 12), Array(14, 16, 18)))
  }

  it should "subract one matrix from another" in {
    val m = VMatrix.fromArray(contents)
    val n = VMatrix.fromArray(Array(Array(7, 8, 9), Array(10, 11, 12)))

    m.subtract(n) shouldBe VMatrix.fromArray(Array(Array(-6, -6, -6), Array(-6, -6, -6)))
  }

  it should "add a new row to a matrix" in {
    val m = VMatrix.fromArray(contents)

    val result = m.addRowFromArray(Array(8, 9, 0))

    result shouldBe VMatrix.fromArray(contents :+ Array(8, 9, 0))
  }
}

class VMatrixCompanionTest extends AnyFlatSpec with Matchers {
  val row1 = Array(1, 2, 3): Array[Double]
  val row2 = Array(4, 5, 6): Array[Double]
  val row3 = Array(7, 8, 9, 10): Array[Double]
  val validTestCSV = "Advertising.csv"

  it should "create a matrix when the row sizes match" in {
    val result = VMatrix.fromArray(Array(row1, row2))

    result shouldBe VMatrix(Vector(row1.toVector, row2.toVector))
  }

  it should "fail when the row sizes do not match" in {
    an[CrimsonError] should be thrownBy VMatrix.fromArray(Array(row1, row2, row3))
  }

  it should "create a matrix from a CSV file" in {
    val result = VMatrix.fromResource(validTestCSV, firstRowHeader = true)

    result.columns shouldBe 5
    result.rows shouldBe 200
  }
}
