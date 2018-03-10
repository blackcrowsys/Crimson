package com.blackcrowsys.crimson.matrix

object Matrix {

  class Matrix(
                val contents: Array[Double],
                val columns: Int) {

    val rows: Int = contents.length / columns

    def *(scaler: Double): Matrix = {
      val result = for (v <- this.contents) yield scaler * v
      Matrix.create(result, this.columns)
    }

    def +(that: Matrix): Matrix = {
      if (this.columns != that.columns || this.contents.length != that.contents.length)
        throw new IllegalArgumentException("Matrix size mismatch")
      val result =
        for ((v1, v2) <- this.contents zip that.contents)
          yield v1 + v2
      Matrix.create(result, this.columns)
    }


    def get(row: Int, col: Int) = {
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

    def equals(that: Matrix): Boolean = {
      if (this.columns != that.columns)
        false
      if (this.contents.length != that.contents.length)
        false
      for ((v1, v2) <- this.contents zip that.contents) {
        if (v1 != v2)
          return false
      }
      true
    }
  }

  def create(contents: Array[Double], cols: Int): Matrix = {
    if (contents.length % cols != 0 || contents.length < cols)
      throw new IllegalArgumentException("Contents mismatch")
    new Matrix(contents, cols)
  }

}
