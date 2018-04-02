package com.blackcrowsys.crimson.statistics

import com.blackcrowsys.crimson.matrix.Matrix

object Stats {

  def median(m: Matrix.Matrix, column: Int): Double = {
    val row = m.getColumnAsArray(column)
    val sorted = row.sortWith(_ < _)
    if (isOdd(sorted.length)) {
      return sorted(Math.floor(sorted.length.toDouble / 2.toDouble).toInt)
    } else {
      return (sorted(sorted.length / 2) + sorted((sorted.length / 2) - 1)) / 2.toDouble
    }
  }

  def isOdd(aNum: Int): Boolean = {
    return !(aNum % 2 == 0)
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
