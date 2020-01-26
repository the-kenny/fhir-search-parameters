import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "de.tarn-vedra"
ThisBuild / organizationName := "fhir-search"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-deprecation",
)

lazy val root = (project in file("."))
  .settings(
    name := "fhir-search",
    libraryDependencies += scalaUri,
    libraryDependencies += scalaTest % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
