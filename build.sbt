name := "ConwayLife"

version := "1.0"

organization := "org.rosettacode.conway_life.scala"

scalaVersion := "2.9.0-1"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-explaintypes")

libraryDependencies += "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"

libraryDependencies <<= (libraryDependencies, scalaVersion) ( _ :+ "org.scala-lang" % "scala-swing" % _ )
