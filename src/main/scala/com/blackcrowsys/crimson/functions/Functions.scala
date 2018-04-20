package com.blackcrowsys.crimson.functions

import scala.math.exp

/**
  * A collection of mathemmatical functions that operate on Double or Matrices.
  */
object Functions {

  /**
    * The Sigmoid function on the Double.
    *
    * @param value the double on which to apply the sigmoid function
    * @return the sigmoid
    */
  def sigmoid(value: Double): Double = {
    1 / (1 + exp(-value))
  }

}
