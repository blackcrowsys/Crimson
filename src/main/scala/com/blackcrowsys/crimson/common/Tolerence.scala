package com.blackcrowsys.crimson.common

import com.blackcrowsys.crimson.matrix.Matrix

class Tolerence(limit: Double) {

  def isWithinTolerence(x: Matrix.Matrix, y: Matrix.Matrix): Boolean = {
    for ((x1, y1) <- x.contents zip y.contents) {
      if (Math.abs(x1 - y1) > limit) return false
    }
    true
  }

}
