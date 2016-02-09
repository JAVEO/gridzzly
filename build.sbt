name := """gridLib"""

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % "2.11.6",
  "com.typesafe.slick" %% "slick" % "3.0.2",
  "com.typesafe.play" %% "play" % "2.4.2"
)

val paradiseVersion = "2.1.0-M5"


addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)