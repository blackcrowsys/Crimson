package com.blackcrowsys.crimson.statistics

import com.blackcrowsys.crimson.matrix.Matrix

object Stats {

  def mean(m: Matrix.Matrix, column: Int): Double = {
    val sum = m.columnSum(column)
    sum / m.rows.toDouble
  }

}
