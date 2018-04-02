package com.blackcrowsys.crimson.ils

import com.blackcrowsys.crimson.common.Tolerence
import com.blackcrowsys.crimson.matrix.Matrix.Matrix

class GradientDescent {

  def iterate(derivative: Matrix => Matrix, initial: Matrix, learningRate: Double, tolerence: Tolerence): Matrix = {
    val zero = initial.*(0)
    var derived: Matrix = derivative.apply(initial)
    if (tolerence.isWithinTolerence(zero, derived)) {
      initial
    } else {
      iterate(derivative, initial + (derived * learningRate * -1), learningRate, tolerence)
    }
  }
}
