package com.blackcrowsys.crimson.ils

import com.blackcrowsys.crimson.common.Tolerence
import com.blackcrowsys.crimson.matrix.Matrix.Matrix

/**
  * Implementation of the gradient descemnt method used for various forms of optimisation. See
  * https://en.wikipedia.org/wiki/Gradient_descent for more information.
  *
  */
class GradientDescent {

  /**
    * The gradient descent iteration on a matrix.
    *
    * @param derivative   the function that supplies the derivative, which should accept a matrix and return the derivative
    *                     at that value
    * @param initial      a matrix representing the starting point
    * @param learningRate the learning rate
    * @param tolerence    the tolerence , i.e. when the derivative is within this margin, we can stop iteration
    * @return the final matrix at which point the derivative is minimised.
    */
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
