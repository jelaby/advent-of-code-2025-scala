ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2025-scala"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.8"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.11.6"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
