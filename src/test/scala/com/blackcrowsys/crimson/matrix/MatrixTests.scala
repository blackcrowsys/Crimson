/**
  * Copyright - see LICENCE
  */
package com.blackcrowsys.crimson.matrix

import org.scalatest.{FunSuite}

class MatrixTests extends FunSuite {

  val x = Matrix.create(Array(12.2, 4, 7, 10), 2)

  test("A double matrix with 2 columns and 2 rows") {
    val ele1 = x.get(1, 1)
    assert(ele1 == 12.2)

    val ele2 = x.get(1, 2)
    assert(ele2 == 4)

    val ele3 = x.get(2, 1)
    assert(ele3 == 7)

    val ele4 = x.get(2, 2)
    assert(ele4 == 10)
  }

  assertThrows[IllegalArgumentException]{
    val e = x.get(1,3)
  }

  assertThrows[IllegalArgumentException]{
    val e = x.get(3, 1)
  }

  assertThrows[IllegalArgumentException]{
    val y = Matrix.create(Array(12.2, 4, 7), 2)
  }

  assertThrows[IllegalArgumentException]{
    val y = Matrix.create(Array(12.1), 2)
  }
}
