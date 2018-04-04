package com.blackcrowsys.crimson.functions

import scala.math.exp

object Functions {
  def sigmoid(value: Double): Double = {
    1 / (1 + exp(-value))
  }

}
