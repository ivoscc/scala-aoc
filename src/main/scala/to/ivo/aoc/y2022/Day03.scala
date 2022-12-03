package to.ivo.aoc.y2022

import to.ivo.aoc.Day

object Day03 extends Day {

  override def part1(lines: List[String]): String = {
    lines
      .foldLeft(0) { (accumulatedScore, line) =>
        val (compartment0, compartment1) = line.splitAt(line.length / 2)
        val commonItem = getCommonItem(compartment0, compartment1)
        accumulatedScore + getItemScore(commonItem)
      }
      .toString // 8185
  }

  override def part2(lines: List[String]): String = {
    lines
      .grouped(3)
      .foldLeft(0) { (accumulatedScore, rucksacks) =>
        val commonItem = getCommonItem(rucksacks: _*)
        accumulatedScore + getItemScore(commonItem)
      }
      .toString // 2817
  }

  private def getCommonItem(strings: String*): Char =
    strings
      .map(_.toSet)
      .foldLeft(Set[Char]()) {
        case (commonItems, items) if commonItems.nonEmpty => commonItems & items
        case (_, items)                                   => items
      }
      .head

  private def getItemScore(item: Char): Int = if (item.isUpper) item - 38 else item - 96

}
