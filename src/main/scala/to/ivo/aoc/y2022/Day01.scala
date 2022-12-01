package to.ivo.aoc.y2022

import to.ivo.aoc.Day

object Day01 extends Day {

  def part1(lines: List[String]): String = {
    val out = lines.foldLeft((0, 0))((acc, line) => {
      if (line.trim.nonEmpty) {
        (acc._1 + line.toInt, acc._2)
      } else {
        val currentSum = acc._1
        val currentMax = acc._2
        (0, currentSum.max(currentMax))
      }
    })
    out._1.max(out._2).toString
  }

  def part2(lines: List[String]): String = {
    val allSums = lines
      .foldLeft((0, List[Int]()))((acc, line) => {
        if (line.trim.nonEmpty) {
          (acc._1 + line.toInt, acc._2)
        } else {
          val currentSum = acc._1
          (0, acc._2 :+ currentSum)
        }
      })
      ._2
    allSums.sorted.takeRight(3).sum.toString
  }

}
