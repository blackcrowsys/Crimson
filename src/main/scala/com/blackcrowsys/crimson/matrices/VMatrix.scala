package com.blackcrowsys.crimson.matrices

import com.blackcrowsys.crimson.errors.CrimsonError

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

/**
  * A Matrix implementation using Vector and Double.
  * If the last column of the matrix is a the result, then it is an augmented matrix.
  * @param matrix the contents of the matrix, a Vector of Vectors of Doubles.
  * @param augmented if the last column is a result, default is false.
  */
case class VMatrix(matrix: Vector[Vector[Double]], augmented: Boolean = false) extends Matrix[Vector[Double], Double] {

  override def rows: Int = matrix.size

  override def columns: Int = getRow(1).size

  override def getRow(row: Int): Vector[Double] = matrix(row - 1)

  override def getColumn(col: Int): Vector[Double] = matrix.map(_ (col - 1))

  override def applyToEach(f: Double => Double): Matrix[Vector[Double], Double] =
    VMatrix(matrix.map(_.map(f(_))))

  override def sumOfSquaredDifference(comparator: Double): Double =
    matrix.map(_.map(d => Math.pow(d - comparator, 2))).map(_.sum).sum

  override def transpose: Matrix[Vector[Double], Double] = {
    @tailrec
    def loop(col: Int, acc: Vector[Vector[Double]]): Vector[Vector[Double]] = {
      if (col > columns)
        acc
      else
        loop(col + 1, acc :+ getColumn(col))
    }

    VMatrix(loop(1, Vector.empty))
  }

  override def dot(that: Matrix[Vector[Double], Double]): Matrix[Vector[Double], Double] = {
    @tailrec
    def loop(thisRow: Int, thatCol: Int, acc: Vector[Vector[Double]]): Vector[Vector[Double]] = {
      if (thisRow > rows && thatCol > that.columns)
        acc
      else if (thatCol > that.columns)
        loop(thisRow + 1, 1, acc)
      else
        loop(thisRow + 1, thatCol + 1, acc :+ zipRow(thisRow))
    }

    def zipRow(row: Int): Vector[Double] = {
      @tailrec
      def loopColumns(colIndex: Int, sumV: Vector[Double]): Vector[Double] = {
        if (colIndex > that.columns)
          sumV
        else {
          val p = for {
            (a, b) <- getRow(row) zip that.getColumn(colIndex)
          } yield (a * b)
          loopColumns(colIndex + 1, sumV :+ p.sum)
        }
      }

      loopColumns(1, Vector.empty)
    }

    VMatrix(loop(1, 1, Vector.empty))
  }

  override def sumOfRow(row: Int): Double = getRow(row).sum

  override def sumOfColumn(col: Int): Double = getColumn(col).sum

  override def sum(that: Matrix[Vector[Double], Double]): Matrix[Vector[Double], Double] = sameSizedMatrixOperation(that, (a, b) => a + b)

  override def subtract(that: Matrix[Vector[Double], Double]): Matrix[Vector[Double], Double] = sameSizedMatrixOperation(that, (a, b) => a - b)

  private def sameSizedMatrixOperation(that: Matrix[Vector[Double], Double], op: (Double, Double) => Double) = {
    def loop(row: Int, acc: Vector[Vector[Double]]): Vector[Vector[Double]] = {
      if (row > rows)
        acc
      else
        loop(row + 1, acc :+ zipRow(row))
    }

    def zipRow(r: Int): Vector[Double] = {
      for {
        (a, b) <- getRow(r) zip that.getRow(r)
      } yield op(a, b)
    }

    VMatrix(loop(1, Vector.empty))
  }

  override def scale(a: Double, b: Double): Double = a * b

  override def addRowFromArray(row: Array[Double]): Matrix[Vector[Double], Double] =
    VMatrix(matrix :+ row.toVector)
}

object VMatrix {

  val CSV_Separator = ","

  def fromResource(resource: String, firstRowHeader: Boolean): Matrix[Vector[Double], Double] = {
    val lines = if(firstRowHeader)
      Source.fromResource(resource).getLines().drop(1)
    else
      Source.fromResource(resource).getLines()
    fromArray(lines.map(convertStringToArray).toArray)
  }

  private def convertStringToArray(line: String): Array[Double] = line.split(CSV_Separator).map(_.toDouble)

  def validate(arrays: Array[Array[Double]]): Array[Array[Double]] = {
    val sizes = arrays.map(_.length)
    if (!sizes.forall(_ == sizes.head))
      throw CrimsonError(CrimsonError.IncorrectRowSize)
    arrays
  }

  def fromArray(arrays: Array[Array[Double]]): Matrix[Vector[Double], Double] =
    VMatrix(validate(arrays).map(_.toVector).toVector)
}
