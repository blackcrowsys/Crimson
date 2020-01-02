name := "Crimson"

version := "1.6.0-SNAPSHOT"

scalaVersion := "2.13.1"

libraryDependencies ++= {
  val scalaTest = "3.1.0"
  val cats = "2.0.0"
  Seq(
    "org.typelevel" %% "cats-core" % cats,
    "org.scalatest" %% "scalatest" % scalaTest % "test"
  )
}