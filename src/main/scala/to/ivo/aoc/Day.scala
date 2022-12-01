package to.ivo.aoc

trait Day {
  def run(input: String): (String, String) = {
    val inputLines = lines(input)
    (part1(inputLines), part2(inputLines))
  }

  def part1(input: List[String]): String

  def part2(input: List[String]): String

  private def lines(input: String) = input.split("\n").toList
}
