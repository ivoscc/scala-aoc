import complete.DefaultParsers._

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "scala-aoc",
    cleanFiles += baseDirectory.value / ".aoc-cache"
  )
