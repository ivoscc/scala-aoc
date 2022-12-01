package to.ivo.aoc

import java.io.FileWriter
import java.nio.file.{Files, Path}
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}
import sys.process._

object Runner {

  val ProjectRoot = new java.io.File(".").getCanonicalPath
  val CookieJar = Path.of(ProjectRoot, "aoc-cookies.txt")
  val CacheDirectory = Path.of(ProjectRoot, ".aoc-cache")

  def main(args: Array[String]) = {
    for {
      _ <- verifyCookieJarExists
      _ <- ensureCacheDirectoryExists
      (year, day) <- parseArgs(args)
      dayClass <- getDayClass(year, day)
      input <- getDayInput(year, day)
    } {
      val (part1Output, part2Output) = dayClass.run(input)
      println(f"Year $year / day $day%02d / part 1 => $part1Output")
      println(f"Year $year / day $day%02d / part 2 => $part2Output")
    }
  }

  private def parseArgs(args: Array[String]): Option[(Int, Int)] = {
    Try(args.map(_.trim.toInt)) match {
      case Success(Array(year, day)) => Some(year, day)
      case _ =>
        println("Please provide a valid year and day to run.")
        None
    }
  }

  private def getDayClass(year: Int, day: Int): Option[Day] = {
    val className = f"to.ivo.aoc.y$year.Day$day%02d"
    Try(
      Class.forName(s"$className$$").getField("MODULE$").get().asInstanceOf[Day]
    ) match {
      case Success(dayObject) => Some(dayObject)
      case Failure(error) =>
        println(s"Failed to load class $className:\n$error")
        None
    }
  }

  private def getDayInput(year: Int, day: Int): Option[String] = {
    val cachedFilename = f"$CacheDirectory/cached-$year-$day%02d-input.txt"
    Using(Source.fromFile(cachedFilename))(_.mkString) match {
      case Success(input) => Some(input)
      case Failure(_) =>
        downloadDayInput(year, day)
          .map { output =>
            Using(new FileWriter(cachedFilename))(_.write(output))
            output
          }
    }
  }

  private def downloadDayInput(year: Int, day: Int): Option[String] = {
    val command =
      s"curl --silent --cookie $CookieJar https://adventofcode.com/$year/day/$day/input"
    Try(command.!!) match {
      case Success(output) => Some(output)
      case Failure(error) =>
        println(s"Failed to retrieve input for $year day $day. Got: \n$error")
        None
    }
  }

  private def verifyCookieJarExists = {
    if (Files.isReadable(CookieJar)) {
      Some()
    } else {
      println("Error: Create a cookies.txt in the project root directory with the AOC credentials.")
      None
    }
  }

  private def ensureCacheDirectoryExists = Try(
    Files.createDirectories(CacheDirectory)
  ) match {
    case Success(_) => Some()
    case Failure(error) =>
      println(s"Error creating cache directory.\n$error")
      None
  }
}
