package to.ivo.aoc.y2022

import to.ivo.aoc.Day

import scala.collection.mutable.Stack
import scala.collection.mutable.Map

object Day07 extends Day {

  override def part1(lines: List[String]): String = {
    parseDirectoryTree(lines)
      .map(_._2)
      .filter(_ < 100_000)
      .sum
      .toString
  }

  override def part2(lines: List[String]): String = {
    val directoryTree = parseDirectoryTree(lines)
    val needToFree = directoryTree("/") - 40_000_000
    directoryTree
      .map(_._2)
      .toList
      .sorted
      .find(needToFree <= _)
      .get
      .toString
  }

  private def parseDirectoryTree(lines: List[String]): Map[String, Int] = {
    val state = Stack[String]()
    val sizes = Map[String, Int]()
    for (line <- lines) {
      line match {
        case s"$$ cd /" =>
          state.popAll()
          state.push("/")
        case s"$$ cd .."        => if (state.length > 1) state.pop()
        case s"$$ cd $target"   => state.push(target)
        case s"dir $_" | "$ ls" => {}
        case s"$size $_" => {
          for (prefixSize <- 1 to state.length) {
            val directory = state.reverse.take(prefixSize).mkString("/").replace("//", "/")
            val updatedSize = sizes.getOrElse(directory, 0) + size.toInt
            sizes(directory) = updatedSize
          }
        }
      }
    }
    sizes
  }
}
