package to.ivo.aoc.y2022

import to.ivo.aoc.Day

object Day04 extends Day {

  override def part1(lines: List[String]): String = {
    lines
      .map(line => line.split(",").flatMap(_.split("-")).map(_.toInt))
      .filter {
        case Array(start0, end0, start1, end1) => {
          (start0 <= start1 && end0 >= end1) || (start1 <= start0 && end1 >= end0)
        }
      }
      .length
      .toString // 588
  }

  override def part2(lines: List[String]): String = {
    lines
      .map(line => line.split(",").flatMap(_.split("-")).map(_.toInt))
      .filter {
        case Array(start0, end0, start1, end1) => {
          (start1 <= start0 && end1 >= start0) ||
          (start0 <= start1 && end0 >= start1)
        }
      }
      .length
      .toString // 911
  }

}
