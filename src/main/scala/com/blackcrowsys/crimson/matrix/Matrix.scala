package com.blackcrowsys.crimson.matrix

import scala.math.floor

object Matrix {

  class Matrix(
                val contents: Array[Double],
                val columns: Int) {
    def getColumnAsArray(column: Int): Array[Double] = {
      def loop(row: Int, acc: Array[Double]): Array[Double] = {
        if (row > rows) acc
        else {
          loop(row + 1, acc :+ get(row, column)
          )
        }
      }

      loop(1, new Array[Double](0))
    }


    def sumOfSquaredDifference(compare: Double, column: Int) = {
      def loop(row: Int, acc: Double): Double = {
        if (row > rows) acc
        else {
          loop(row + 1, acc + Math.pow(get(row, column) - compare, 2))
        }
      }

      loop(1, 0.toDouble)
    }

    def apply(function: Double => Double): Matrix = {
      val results = for (v <- this.contents) yield function.apply(v)
      Matrix.create(results, this.columns)
    }


    val rows: Int = contents.length / columns

    def transpose: Matrix = {
      val results = Array.ofDim[Double](this.rows * this.columns)
      for (index <- 0 until results.length) {
        results(index) = getTransposeFromIndex(index)
      }
      Matrix.create(results, this.rows)
    }

    private def getTransposeFromIndex(index: Int): Double = {
      val (row: Int, col: Int) = getCoords(index, this.columns, this.rows)
      get(col, row)
    }

    def dotProduct(that: Matrix): Matrix = {
      val results = Array.ofDim[Double](this.rows * that.columns)

      for (index <- 0 until results.length) {
        results(index) = calculateSum(index, this.rows, that.columns, that)
      }

      Matrix.create(results, that.columns)
    }

    def calculateSum(index: Int, rows: Int, columns: Int, that: Matrix): Double = {
      val (row: Int, col: Int) = getCoords(index, rows, columns)

      var sum: Double = 0
      for (i <- 1 to this.columns) {
        sum = sum + this.get(row, i) * that.get(i, col)
      }
      sum
    }

    private def getCoords(index: Int, rows: Int, columns: Int): (Int, Int) = {
      val row = (floor(index / columns) + 1).toInt
      val colIndex = (index + 1) % columns
      val col = if (colIndex == 0) columns else colIndex
      (row, col)
    }

    def *(that: Matrix): Matrix = dotProduct(that)


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

    def get(row: Int, col: Int): Double = {
      if (col > columns || col < 1) {
        throw new IllegalArgumentException("Incorrect Column Index: " + col)
      }
      if (row > contents.length / columns || row < 1) {
        throw new IllegalArgumentException("Incorrect Row Index: " + row)
      }
      contents(getIndex(row, col, this.rows, this.columns))
    }

    private def getIndex(row: Int, col: Int, rows: Int, columns: Int): Int = {
      (row - 1) * columns + (col - 1)
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

    def columnSum(column: Int): Double = {
      def loop(row: Int, acc: Double): Double = {
        if (row > rows) acc
        else {
          loop(row + 1, acc + get(row, column))
        }
      }

      loop(1, 0.0)
    }
  }

  def create(contents: Array[Double], cols: Int): Matrix = {
    if (contents.length % cols != 0 || contents.length < cols)
      throw new IllegalArgumentException("Contents mismatch")
    new Matrix(contents, cols)
  }

}
