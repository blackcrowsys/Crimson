package com.blackcrowsys.crimson.matrix

import com.blackcrowsys.crimson.common.Tolerence

import scala.io.Source
import scala.math.floor
import scala.util.Random

/**
  * A simple matrix using an array of Double to hold values of double.
  * The maximum size of the matrix is that of the Array, 2^31 - 1 or 2,147,483,647.
  * The matrix array is row based. If the following is matrix:
  * | 1  2 |
  * | 3  4 |
  * then the array stores this as [1, 2, 3, 4].
  * Such as the matrix above has the following properties:
  * It has two columns, the first column is 1 and the second column is 2, not 0 and 1 respectively.
  * It has two rows, the first row is 1, and the second row is 2, not 0 and 1 respectively.
  * The array is treated as immutable. Any operations on a matrix will result in another matrix without mutating the
  * values in the matrix.
  * Although the array is visible outside of the matrix, as one of it's field, and can be mutated, for now, you SHOULD
  * NOT change it as this will be fixed in future release.
  **/
@deprecated
object Matrix {

  /**
    * Constructor for the matrix.
    *
    * @param contents the array containing the row-based values of matrix
    * @param columns  the number of columns in the matrix
    */
  @deprecated
  class Matrix(val contents: Array[Double], val columns: Int) {

    val rows: Int = contents.length / columns

    /**
      * Returns the contents of a matrix as an array of rows.
      *
      * @return an array of rows
      */
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

    /**
      * Returns the column as an array.
      *
      * @param column the column
      * @return the column as an array
      */
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

    /**
      * Gives a square of the difference between a given number and all the values of a matrix for a column.
      * So, if the following is the given column for a matrix: [ a, b] and c is the comparator, then this will give
      * the following output:
      * (a - c)^2 + (b - c)^2
      *
      * @param compare the given number to compare, or subtract, against
      * @param column  the column to use
      * @return the sum of the sqaure of the difference
      */
    def sumOfSquaredDifference(compare: Double, column: Int): Double = {
      def loop(row: Int, acc: Double): Double = {
        if (row > rows) acc
        else {
          loop(row + 1, acc + Math.pow(get(row, column) - compare, 2))
        }
      }

      loop(1, 0.toDouble)
    }

    /**
      * Applies a function to each of the values in the matrix.
      *
      * @param function the function to apply
      * @return a matrix with the new values after applying the function
      */
    def apply(function: Double => Double): Matrix = {
      val results = for (v <- this.contents) yield function.apply(v)
      Matrix.create(results, this.columns)
    }

    /**
      * Gets a transpose of a matrix.
      *
      * @return the transpose of the matrix
      */
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

    /**
      * Returns the dot product. If this is A and B is the given matrix, then it will return
      * A . B
      *
      * @param that the matrix to use for dot product
      * @return the result of dot product
      */
    def dot(that: Matrix): Matrix = {
      val results = Array.ofDim[Double](this.rows * that.columns)

      for (index <- 0 until results.length) {
        results(index) = calculateSum(index, this.rows, that.columns, that)
      }

      Matrix.create(results, that.columns)
    }

    private def calculateSum(index: Int, rows: Int, columns: Int, that: Matrix): Double = {
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

    /**
      * Multiplies the elements with elements in another identical size matrix, ie:
      * [a, b] * [c, d] = [a*b, c*d]
      *
      * @param that the matrix to multiply elements with
      * @return result matrix
      */
    def *(that: Matrix): Matrix = {
      if (this.columns != that.columns || this.contents.length != that.contents.length)
        throw new IllegalArgumentException("Matrix size mismatch")
      val result =
        for ((v1, v2) <- this.contents zip that.contents)
          yield v1 * v2
      Matrix.create(result, this.columns)
    }


    /**
      * Scalar multiplication of a matrix with a given number, i.e.:
      * a * [b, c] = [a*b, a*c]
      *
      * @param scaler the scalar value
      * @return the resulting matrix
      */
    def *(scaler: Double): Matrix = {
      val result = for (v <- this.contents) yield scaler * v
      Matrix.create(result, this.columns)
    }

    /**
      * Adds another identical size matrix with this one.
      *
      * @param that the identical size matrix to add with
      * @return the resulting matrix
      */
    def +(that: Matrix): Matrix = {
      if (this.contents.length != that.contents.length || this.columns != that.columns)
        throw new IllegalArgumentException("Matrix size mismatch")
      val result =
        for ((v1, v2) <- this.contents zip that.contents)
          yield v1 + v2
      Matrix.create(result, this.columns)
    }

    /**
      * Subtracts another identical size matrix with this one.
      *
      * @param that the identical matrix to subtract
      * @return the resulting matrix
      */
    def -(that: Matrix): Matrix = {
      if (this.contents.length != that.contents.length || this.columns != that.columns)
        throw new IllegalArgumentException("Matrix size mismatch")
      val result =
        for ((v1, v2) <- this.contents zip that.contents)
          yield v1 - v2
      Matrix.create(result, this.columns)
    }

    /**
      * Gets the value at a given column and row.
      *
      * @param row the row
      * @param col the column
      * @return the value at that (column, row)
      */
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

    /**
      * Compares if this matrix is equal to another by value
      *
      * @param that the matrix to compare to
      * @return true if each matrix have identical values and size, otherwise false
      */
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

    /**
      * Compares if this matrix is equal to another by value of each element if within a tolerence.
      *
      * @param that      the matrix to compare to
      * @param tolerence the tolerance
      * @return true if equal within tolerence, otherwise false
      */
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

    /**
      * Gets the sum of all elements in a given column.
      *
      * @param column the column
      * @return the sum of elements in the column
      */
    def columnSum(column: Int): Double = {
      def loop(row: Int, acc: Double): Double = {
        if (row > rows) acc
        else {
          loop(row + 1, acc + get(row, column))
        }
      }

      loop(1, 0.0)
    }

    /**
      * The string representation of this matrix.
      *
      * @return the string representation
      */
    override def toString: String = {
      val rowList: Array[String] = for (row <- this.rowArray()) yield row.mkString(", ")
      rowList.mkString("||")
    }
  }

  /**
    * Creates a new matrix.
    *
    * @param contents the array containing row based elements
    * @param cols     the column size
    * @return the matrix
    */
  def create(contents: Array[Double], cols: Int): Matrix = {
    if (contents.length % cols != 0 || contents.length < cols)
      throw new IllegalArgumentException("Contents mismatch")
    new Matrix(contents, cols)
  }

  /**
    * Creates a matrix with identical values.
    *
    * @param rows    the number of rows
    * @param columns the number of columns
    * @param value   the value for each element
    * @return the matrix with the required value
    */
  def create(rows: Int, columns: Int, value: Double): Matrix = {
    val contentArray: Array[Double] = (for (i <- 0 until rows * columns) yield value).toArray
    create(contentArray, columns)
  }

  /**
    * Creates a matrix but wiith random values.
    * The random values for each elements is set by the formula:
    * r * m - s,
    * where r is the random value generated, between 0 and 1,
    * m and s are the given parameters
    *
    * @param rows       the number of rows
    * @param columns    the number of columns
    * @param multiplier the multiplier, m
    * @param subractor  the subtractor, s
    * @return the matrix
    */
  def createRandom(rows: Int, columns: Int, multiplier: Int, subractor: Int): Matrix = {
    val contentArray: Array[Double] = (for (i <- 0 until rows * columns) yield Random.nextDouble() * multiplier - subractor)
      .toArray
    create(contentArray, columns)
  }

  /**
    * Creates a matrix from a CSV file located in the resources location.
    * The CSV file can have the first line as a header, in which case it will be dropped.
    * The CSV file can have the first column as the index of the row, in which case it will be dropped.
    *
    * @param filename        the name of the file that must be in the resources folder
    * @param firstLineHeader if the file line is a header
    * @param indexColumn     if the first column is the index
    * @return the matrix
    */
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

  private def convertFromString(stringValues: Array[String]): Array[Double] = {
    for (s <- stringValues) yield s.toDouble
  }

}
