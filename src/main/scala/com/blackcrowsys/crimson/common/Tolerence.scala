package com.blackcrowsys.crimson.common

import com.blackcrowsys.crimson.matrix.Matrix

/**
  * Tolerence class is used to define the limits at which an answer is acceptable. For example, imagine an iterative
  * algorithm that converges to A. At what point do we stop? Tolerence is used to define the acceptable limit at which
  * we can stop. For example, if Y is the result of an iterative algorithm, and T is the tolerence that we have
  * specified, then when |A - Y| < T, the iterative processing can stop. T is the same as ε, i.e.:
  * |A - Y| < ε
  *
  * @param limit the error limit ε
  */
class Tolerence(limit: Double) {

  /**
    * Gives the answer for two matrices, X and Y, if they are within the limit, i.e.  if |X - Y| < ε
    * Note: If either one or both matrices are null, or if matrices are not of the same size, then it will return false.
    *
    * @param x the first Matrix
    * @param y the second matrix
    * @return true if |X - Y| < ε otherwise false
    */
  def isWithinTolerence(x: Matrix.Matrix, y: Matrix.Matrix): Boolean = {
    if (x == null || y == null) return false
    if (x.columns != y.columns || x.rows != y.rows) return false
    for ((x1, y1) <- x.contents zip y.contents) {
      if (Math.abs(x1 - y1) > limit) return false
    }
    true
  }

}
