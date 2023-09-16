ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file(".")).settings(
  name := "mazes"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.10.0",
  "org.typelevel" %% "cats-effect" % "3.5.1",
)
scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Yexplicit-nulls",
  "-Ykind-projector:underscores",
  "-Ysafe-init",
  "-source:future",
)
