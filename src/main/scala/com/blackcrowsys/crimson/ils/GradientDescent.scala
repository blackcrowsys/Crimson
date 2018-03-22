package com.blackcrowsys.crimson.ils

import javax.swing.plaf.BorderUIResource.MatteBorderUIResource

import com.blackcrowsys.crimson.common.Tolerence
import com.blackcrowsys.crimson.matrix.Matrix
import com.blackcrowsys.crimson.matrix.Matrix.{Matrix, create}

class GradientDescent(val derivative: Matrix => Matrix,
                      val initial: Matrix,
                      val learningRate: Double,
                      val tolerence: Tolerence) {

  def interate(): Matrix = {
    val zero = initial.*(0)
    var derived: Matrix = derivative.apply(initial)
    if (tolerence.isWithinTolerence(zero, derived)) return initial

    var next = initial.*(1)

    while (!tolerence.isWithinTolerence(derived, zero)) {
      derived = derivative.apply(next)
      var change = derived.*(learningRate).*(-1)
      next = next + change
    }
    next
  }

}
