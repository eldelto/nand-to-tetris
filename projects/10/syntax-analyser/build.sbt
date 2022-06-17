import Dependencies._

ThisBuild / scalaVersion := "3.1.1"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "net.eldelto"
ThisBuild / organizationName := "syntax-analyzer"

lazy val root = (project in file("."))
  .settings(
    name := "vm-translator",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0",
    libraryDependencies += "org.apache.commons" % "commons-text" % "1.9",

    libraryDependencies += scalaTest % Test,

    // Build
    assembly / mainClass := Some("net.eldelto.nand2tetris.syntaxanalyzer.Main"),
    assembly / assemblyJarName := "vm-translator.jar",

    // Test
    // Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oF")
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
