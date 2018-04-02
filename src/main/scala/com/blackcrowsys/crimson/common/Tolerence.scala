package com.blackcrowsys.crimson.common

import com.blackcrowsys.crimson.matrix.Matrix

class Tolerence(limit: Double) {

  def isWithinTolerence(x: Matrix.Matrix, y: Matrix.Matrix): Boolean = {
    if (x == null || y == null) return false
    if (x.columns != y.columns || x.rows != y.rows) return false
    for ((x1, y1) <- x.contents zip y.contents) {
      if (Math.abs(x1 - y1) > limit) return false
    }
    true
  }

}
