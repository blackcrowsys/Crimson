/**
  * Copyright - see LICENCE
  */
package com.blackcrowsys.crimson.matrix

object Matrix {

  class Matrix[T](
                   val contents: Array[T],
                   val columns: Int
                 ) {
    def get(row: Int, col: Int): T = {
      if (col > columns) {
        throw new IllegalArgumentException("Column Index exceeds number of columns: " + col)
      }
      if (row > contents.length / columns) {
        throw new IllegalArgumentException("Row Index exceeds number of rows: " + row)
      }
      val subtractor = if (row == 1 && col == 1) 1
      else if (col == 1) 0
      else 1
      contents(row * col - subtractor)
    }

  }


  def create(contents: Array[Double], columns: Int): Matrix[Double] = {
    if (contents.length % columns != 0 || contents.length < columns)
      throw new IllegalArgumentException("Contents mismatch")
    new Matrix[Double](contents, columns)
  }
}
