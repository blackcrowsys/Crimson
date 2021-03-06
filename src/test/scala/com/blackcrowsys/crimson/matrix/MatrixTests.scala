package com.blackcrowsys.crimson.matrix

import com.blackcrowsys.crimson.common.Tolerence
import org.scalatest.FunSuite

class MatrixTests extends FunSuite {

  val x: Matrix.Matrix = Matrix.create(Array(12.5, 4.8, 3.4, 2), 2)
  val y: Matrix.Matrix = Matrix.create(Array(2, 4, 6, 8), 2)
  val z: Matrix.Matrix = Matrix.create(Array(2, 5), 1)
  val t: Matrix.Matrix = Matrix.create(Array(3, 5, 7, 6, 5, 1), 3)
  val expected: Matrix.Matrix = Matrix.create(Array(14.5, 8.8, 9.4, 10), 2)
  val l = Matrix.create(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2), 3)

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
    assert(!x.equals(y))
  }

  test("it can add two matrix together") {
    val actual = x + y
    assert(actual.equals(expected))
  }

  test("it can subtract one matrix from another") {
    val actual = x - y
    val tolerence: Tolerence = new Tolerence(0.00001)
    assert(Matrix.create(Array(10.5, 0.8, -2.6, -6), 2).equals(actual, tolerence))
  }

  assertThrows[IllegalArgumentException] {
    x + z
  }

  test("dot product with uneven but matched matrices") {
    val actual = x.dot(z)

    assert(Matrix.create(Array(49, 16.8), 1).equals(actual))
  }

  test("dot product with two matched matrices") {
    val actual = x.dot(y)
    assert(Matrix.create(Array(53.8, 88.4, 18.8, 29.6), 2).equals(actual))
  }

  test("scalar multiplication of a matrix") {
    val actual = x * 2

    assert(Matrix.create(Array(25, 9.6, 6.8, 4), 2).equals(actual))
  }

  test("multiplication of two identical matrices") {
    val actual = x * y
    assert(Matrix.create(Array(25, 19.2, 20.4, 16), 2).equals(actual))
  }

  test("transposing a matrix") {
    val m = Matrix.create(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 3)
    val transpose = m.transpose

    assert(Matrix.create(Array(1, 4, 7, 10, 2, 5, 8, 11, 3, 6, 9, 12), 3).equals(transpose))
  }

  test("applying a function to a matrix") {
    val function = (x: Double) => Math.pow(x, 2)
    val actual = y.apply(function)

    assert(Matrix.create(Array(4, 16, 36, 64), 2).equals(actual))
  }

  test("sum of a column in a matrix") {
    assert(z.columnSum(1) == 7.0)
    assert(y.columnSum(1) == 8.0)
    assert(y.columnSum(2) == 12.0)
    assert(t.columnSum(1) == 9.0)
    assert(t.columnSum(2) == 10.0)
    assert(t.columnSum(3) == 8.0)
  }

  test("sum of squared difference from a given number") {
    val actual = l.sumOfSquaredDifference(5.toDouble, 1)
    assert(actual == 46.toDouble)
  }

  test("it gives a column in a list") {
    val actual: Array[Double] = x.getColumnAsArray(1)
    assert(actual(0) == 12.5)
    assert(actual(1) == 3.4)
    assert(actual.length == 2)
  }

  test("it can create a matrix from a file") {
    val matrix: Matrix.Matrix = Matrix.createFromResourceFile("Advertising.csv", true, true)
    assert(matrix != null)
    assert(matrix.columns == 4)
    assert(matrix.rows == 200)
  }

  test("it can give an array of row data") {
    val rows: Array[Array[Double]] = y.rowArray()

    assert(rows.length == 2)
    val row1: Array[Double] = rows(0)
    val row2: Array[Double] = rows(1)

    assert(row1.length == 2)
    assert(row1(0) == 2)
    assert(row1(1) == 4)
    assert(row2(0) == 6)
    assert(row2(1) == 8)
  }

  test("it can create a matrix populated with a give value") {
    val ROWS: Int = 3
    val COLS: Int = 4
    val matrix: Matrix.Matrix = Matrix.create(ROWS, COLS, 0.toDouble)
    assert(matrix.rows == ROWS)
    assert(matrix.columns == COLS)
    assert(0 == matrix.contents.sum)
  }
  
  test("it can create a matrix with random values") {
    val ROWS: Int = 3
    val COLS: Int = 4
    val MULTIPLIER = 2
    val SUBTRACTOR = 1
    val matrix: Matrix.Matrix = Matrix.createRandom(ROWS, COLS, MULTIPLIER, SUBTRACTOR)

    assert(matrix.rows == ROWS)
    assert(matrix.columns == COLS)
    assert(matrix.get(ROWS, COLS) < 1 && matrix.get(ROWS, COLS) > -1)
  }

  test("to string") {
    val expected: String = "2.0, 4.0||6.0, 8.0"
    val actual = y.toString
    assert(expected == actual)
  }
}
