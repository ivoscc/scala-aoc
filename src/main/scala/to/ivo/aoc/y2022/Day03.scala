package to.ivo.aoc.y2022

import to.ivo.aoc.Day

object Day03 extends Day {

  private def inBoth(s0: String, s1: String): Char =
    s0.toSet.intersect(s1.toSet).head

  private def inAll(s0: String, s1: String, s2: String): Char =
    s0.toSet.intersect(s1.toSet).intersect(s2.toSet).head

  private def scoreChar(c: Char): Int = if (c.isUpper) {
    c - 38
  } else {
    c - 96
  }

  override def part1(lines: List[String]): String = {
    lines
      .foldLeft(0) { (sum, line) =>
        val (compartment0, compartment1) = line.splitAt(line.length / 2)
        val commonItem = inBoth(compartment0, compartment1)
        sum + scoreChar(commonItem)
      }
      .toString
  }

  override def part2(lines: List[String]): String = {
    lines
      .grouped(3)
      .foldLeft(0) {
        case (sum, List(g0, g1, g2)) => {
          val commonItem = inAll(g0, g1, g2)
          sum + scoreChar(commonItem)
        }
        case _ => 0
      }
      .toString
  }

}
