package com.blackcrowsys.crimson.statistics

import com.blackcrowsys.crimson.matrix.Matrix

object Stats {
  def percentile(m: Matrix.Matrix, column: Int, score: Double): Double = {
    val sorted = getSortedColumn(m, column)
    val index: Int = Math.ceil(sorted.length * score / 100).toInt
    sorted(
      if (index == 0)
        0
      else
        index - 1
    )
  }


  def median(m: Matrix.Matrix, column: Int): Double = {
    val sorted = getSortedColumn(m, column)
    if (isOdd(sorted.length)) {
      sorted(Math.floor(sorted.length.toDouble / 2.toDouble).toInt)
    } else {
      (sorted(sorted.length / 2) + sorted((sorted.length / 2) - 1)) / 2.toDouble
    }
  }

  private def getSortedColumn(m: Matrix.Matrix, column: Int): Array[Double] = {
    m.getColumnAsArray(column).sortWith(_ < _)
  }

  def isOdd(aNum: Int): Boolean = {
    !(aNum % 2 == 0)
  }

  def sd(m: Matrix.Matrix, column: Int): Double = {
    Math.sqrt(variance(m, column))
  }

  def variance(m: Matrix.Matrix, column: Int): Double = {
    val sum = m.sumOfSquaredDifference(mean(m, column), column)
    sum / (m.rows - 1).toDouble
  }

  def mean(m: Matrix.Matrix, column: Int): Double = {
    val sum = m.columnSum(column)
    sum / m.rows.toDouble
  }
}
