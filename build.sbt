import sbt.Keys.libraryDependencies

val commonSettings = Seq(
  scalaVersion := "2.12.2",

  libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.12" % "3.0.1"
  )
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
