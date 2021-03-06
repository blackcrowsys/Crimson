package com.blackcrowsys.crimson.statistics

import com.blackcrowsys.crimson.matrix.Matrix
import org.scalatest.FunSuite

class StatisticsTests extends FunSuite {

  val x: Matrix.Matrix = Matrix.create(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2), 4)
  val y: Matrix.Matrix = Matrix.create(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2), 3)

  test("it gets the mean for the first column") {
    val expected: Double = 5
    val actual: Double = Stats.mean(x, 1)
    assert(actual == expected)
  }
  test("it gets the mean for the second column") {
    val expected: Double = 8.toDouble / 3.toDouble;
    val actual: Double = Stats.mean(x, 2)
    assert(actual == expected)
  }
  test("it gets the mean for the third column") {
    val expected: Double = 11.toDouble / 3.toDouble;
    val actual: Double = Stats.mean(x, 3)
    assert(actual == expected)
  }

  test("it gets the mean for the forth column") {
    val expected: Double = 14.toDouble / 3.toDouble
    val actual: Double = Stats.mean(x, 4)
    assert(actual == expected)
  }

  test("it gets the variance (standard deviation squared)") {
    val exptected: Double = 16
    val actual: Double = Stats.variance(x, 1)
    assert(actual == exptected)
  }

  test("it gets the standard deviation") {
    val expected: Double = 4
    val actual: Double = Stats.sd(x, 1)
    assert(actual == expected)
  }

  test("it should get the median of odd numbered row matrix") {
    val test = Matrix.create(Array(7, 5, 6, 4, 5, 6, 9, 0, 1), 3)
    val expected: Double = 7
    val actual: Double = Stats.median(test, 1)
    assert(actual == expected)
  }

  test("it should get the median of even numbered row matrix") {
    val test = Matrix.create(Array(7, 5, 6, 4, 5, 6, 0, 1), 2)
    val expected: Double = 5.5
    val actual: Double = Stats.median(test, 1)
    assert(actual == expected)
  }

  test("it should get the given percentile") {
    val test = Matrix.create(Array(7, 5, 6, 4, 5, 6, 0, 1, 2, 3, 4, 5, 7, 3, 1, 2, 0, 9, 8, 9), 2)

    assert(Stats.percentile(test, 1, 0) == 0.toDouble)
    assert(Stats.percentile(test, 1, 10) == 0.toDouble)
    assert(Stats.percentile(test, 1, 70) == 6.toDouble)
    assert(Stats.percentile(test, 1, 80) == 7.toDouble)
    assert(Stats.percentile(test, 1, 90) == 7.toDouble)
    assert(Stats.percentile(test, 1, 100) == 8.toDouble)

    assert(Stats.percentile(test, 2, 0) == 1.toDouble)
    assert(Stats.percentile(test, 2, 10) == 1.toDouble)
    assert(Stats.percentile(test, 2, 70) == 5.toDouble)
    assert(Stats.percentile(test, 2, 80) == 6.toDouble)
    assert(Stats.percentile(test, 2, 90) == 9.toDouble)
    assert(Stats.percentile(test, 2, 100) == 9.toDouble)
  }

}
