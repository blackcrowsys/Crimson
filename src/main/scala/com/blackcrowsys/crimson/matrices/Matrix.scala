package com.blackcrowsys.crimson.matrices

/**
  * A matrix trait that defines operations on a matrix based on:
  * @tparam S - the collection type
  * @tparam N - the number type
  */
trait Matrix[S, N] {

  def rows: Int

  def columns: Int

  def getRow(row: Int): S

  def getColumn(col: Int): S

  def applyToEach(f: N => N): Matrix[S, N]

  def sumOfSquaredDifference(comparator: Double): Double

  def transpose: Matrix[S, N]

  def scale(a: N, b: N): N

  def scalar(multiplier: N): Matrix[S, N] = applyToEach(a => scale(a, multiplier))

  def dot(that: Matrix[S, N]): Matrix[S, N]

  def sumOfRow(row: Int):Double

  def sumOfColumn(col: Int): Double

  def sum(that: Matrix[S, N]): Matrix[S, N]

  def subtract(that: Matrix[S, N]): Matrix[S, N]

  def addRowFromArray(row: Array[N]): Matrix[S, N]

}
