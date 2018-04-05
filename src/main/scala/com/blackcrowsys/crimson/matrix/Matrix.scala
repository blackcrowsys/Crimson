package com.blackcrowsys.crimson.matrix

import com.blackcrowsys.crimson.common.Tolerence

import scala.io.Source
import scala.math.floor

object Matrix {

  class Matrix(val contents: Array[Double], val columns: Int) {

    val rows: Int = contents.length / columns

    def rowArray(): Array[Array[Double]] = {
      var from: Int = 0
      var to: Int = this.columns
      val rowArray: Array[Array[Double]] = Array.ofDim(this.rows)
      for (i <- 0 until rows) {
        rowArray(i) = contents.slice(from, to)
        from += this.columns
        to += this.columns
      }
      rowArray
    }

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

    def sumOfSquaredDifference(compare: Double, column: Int): Double = {
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

    def dot(that: Matrix): Matrix = {
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

    def *(that: Matrix): Matrix = {
      if (this.columns != that.columns || this.contents.length != that.contents.length)
        throw new IllegalArgumentException("Matrix size mismatch")
      val result =
        for ((v1, v2) <- this.contents zip that.contents)
          yield v1 * v2
      Matrix.create(result, this.columns)
    }


    def *(scaler: Double): Matrix = {
      val result = for (v <- this.contents) yield scaler * v
      Matrix.create(result, this.columns)
    }

    def +(that: Matrix): Matrix = {
      if (this.contents.length != that.contents.length || this.columns != that.columns)
        throw new IllegalArgumentException("Matrix size mismatch")
      val result =
        for ((v1, v2) <- this.contents zip that.contents)
          yield v1 + v2
      Matrix.create(result, this.columns)
    }

    def -(that: Matrix): Matrix = {
      if (this.contents.length != that.contents.length || this.columns != that.columns)
        throw new IllegalArgumentException("Matrix size mismatch")
      val result =
        for ((v1, v2) <- this.contents zip that.contents)
          yield v1 - v2
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

    def equals(that: Matrix, tolerence: Tolerence): Boolean = {
      if (this.columns != that.columns)
        false
      if (this.contents.length != that.contents.length)
        false
      for ((v1, v2) <- this.contents zip that.contents) {
        if (!tolerence.isWithinTolerence(this, that))
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

  def create(rows: Int, columns: Int, value: Double): Matrix = {
    val contentArray: Array[Double] = (for (i <- 0 until rows * columns) yield value).toArray
    create(contentArray, columns)
  }

  def createFromResourceFile(filename: String, firstLineHeader: Boolean, indexColumn: Boolean): Matrix = {
    val source = Source.fromResource(filename)
    var skipped = false
    var columns = 0
    var contents: Array[Double] = Array()
    for (line <- source.getLines()) {
      if (firstLineHeader && !skipped)
        skipped = true
      else {
        var tokens: Array[String] = line.split(",")
        if (indexColumn)
          tokens = tokens.drop(1)
        val values: Array[Double] = convertFromString(tokens)
        if (columns == 0)
          columns = tokens.length
        contents = contents ++ values
      }
    }
    source.close()
    create(contents, columns)
  }

  def convertFromString(stringValues: Array[String]): Array[Double] = {
    for (s <- stringValues) yield s.toDouble
  }

}
