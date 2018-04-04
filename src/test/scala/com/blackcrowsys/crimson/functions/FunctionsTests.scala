package com.blackcrowsys.crimson.functions

import org.scalatest.FunSuite

class FunctionsTests extends FunSuite {

  test("sigmoid function") {
    assert(Functions.sigmoid(-100) < 0.0000001)
    assert(Functions.sigmoid(-1) == 0.2689414213699951)
    assert(Functions.sigmoid(0.toDouble) == 0.5)
    assert(Functions.sigmoid(1) == 0.7310585786300049)
    assert(Functions.sigmoid(100) == 1)
  }

}
