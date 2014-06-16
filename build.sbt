name := "fp-in-scala"

version := "1.0"

scalaVersion := "2.10.2"

scalariformSettings

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

scalacOptions ++= Seq("-feature")
