package com.blackcrowsys.crimson.errors

case class CrimsonError(msg: String) extends RuntimeException

object CrimsonError {
  val IncorrectRowSize = "Rows are of different sizes"
}
