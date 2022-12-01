package to.ivo.aoc.y2022

import to.ivo.aoc.Day

object Day01 extends Day {

  override def part1(lines: List[String]): String = {
    val (maxSum, currentSum) = lines.foldLeft((0, 0)) {
      case ((currentMax, currentSum), line) if line.trim.nonEmpty => (currentMax, currentSum + line.toInt)
      case ((currentMax, currentSum), _)                          => (currentMax.max(currentSum), 0)
    }
    maxSum.max(currentSum).toString // 68929
  }

  override def part2(lines: List[String]): String = {
    val allSums = lines
      .foldLeft((0, List[Int]())) {
        case ((currentSum, allSums), line) if line.trim.nonEmpty => (currentSum + line.toInt, allSums)
        case ((currentSum, allSums), _)                          => (0, allSums :+ currentSum)
      }
      ._2
    allSums.sorted.takeRight(3).sum.toString // 203203
  }

}
