ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "VaLeaDate",
    libraryDependencies ++= Seq(
      "io.github.leoprover" %% "scala-tptp-parser" % "1.7.4",
      "com.github.scopt" %% "scopt" % "4.1.0"
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked"
    ),
    assembly / mainClass := Some("Main"),
    assembly / assemblyJarName := "valeadate.jar"
  )
