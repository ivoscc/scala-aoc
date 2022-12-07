package to.ivo.aoc

trait Day {
  def run(input: String): (String, String) = {
    val inputLines = lines(input)
    (part1(inputLines), part2(inputLines))
  }

  def part1(lines: List[String]): String

  def part2(lines: List[String]): String

  private def lines(input: String) = input.split("\n").toList
}
