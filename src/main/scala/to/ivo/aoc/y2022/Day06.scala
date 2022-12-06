package to.ivo.aoc.y2022

import to.ivo.aoc.Day

object Day06 extends Day {
  override def part1(lines: List[String]): String =
    getFirstMarkerPosition(lines(0), 4) // 1625

  override def part2(lines: List[String]): String =
    getFirstMarkerPosition(lines(0), 14) // 2250

  private def getFirstMarkerPosition(stream: String, windowSize: Int): String =
    (stream
      .sliding(windowSize)
      .indexWhere(_.toSet.size == windowSize) + windowSize).toString
}
