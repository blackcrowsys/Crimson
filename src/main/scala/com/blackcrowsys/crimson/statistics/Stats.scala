package com.blackcrowsys.crimson.statistics

import com.blackcrowsys.crimson.matrix.Matrix

/**
  * The class contain statistical functions.
  */
object Stats {

  /**
    * Gets the value at the given percentile.
    *
    * @param m      the matrix
    * @param column the column to check against
    * @param score  the percentile
    * @return the value at that percentile
    */
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

  /**
    * Calculates the median of a matrix for a given column.
    *
    * @param m      the matrix
    * @param column the column
    * @return the median
    */
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

  /**
    * Okay, not a statitistical function, not sure how it ended up here so should be moved to another location...
    * Checks if the given number is odd.
    *
    * @param aNum the number to check
    * @return true if odd, otherwise false
    */
  def isOdd(aNum: Int): Boolean = {
    !(aNum % 2 == 0)
  }

  /**
    * Calculates the standard deviation, σ, for a given column of a matrix.
    *
    * @param m      the matrix
    * @param column the column of the matrix for which σ is to be calculated
    * @return the σ value
    */
  def sd(m: Matrix.Matrix, column: Int): Double = {
    Math.sqrt(variance(m, column))
  }

  /**
    * Calculates the variance for a given column of a matrix.
    *
    * @param m      the matrix
    * @param column the column of the matrix for which variance is to be calculated
    * @return the variance
    */
  def variance(m: Matrix.Matrix, column: Int): Double = {
    val sum = m.sumOfSquaredDifference(mean(m, column), column)
    sum / (m.rows - 1).toDouble
  }

  /**
    * Calculates the mean for a given column of a matrix.
    *
    * @param m      the matrix
    * @param column the column for which the mean is to be calculated
    * @return the mean
    */
  def mean(m: Matrix.Matrix, column: Int): Double = {
    val sum = m.columnSum(column)
    sum / m.rows.toDouble
  }
}
