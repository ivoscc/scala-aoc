package to.ivo.aoc.y2022

import to.ivo.aoc.Day

object Day02 extends Day {
  // rock =     x a     1
  // paper =    y b     2
  // scissors = z c     3
  private def getScoreDiff(playA: String, playB: String): Int = {
    val winScore = 6;
    val drawScore = 3;
    val loseScore = 0;
    val values = Map("X" -> 1, "Y" -> 2, "Z" -> 3)

    (playA, playB) match {
      case ("A", "X") => drawScore + values.get("X").get
      case ("A", "Y") => winScore + values.get("Y").get
      case ("A", "Z") => loseScore + values.get("Z").get
      case ("B", "X") => loseScore + values.get("X").get
      case ("B", "Y") => drawScore + values.get("Y").get
      case ("B", "Z") => winScore + values.get("Z").get
      case ("C", "X") => winScore + values.get("X").get
      case ("C", "Y") => loseScore + values.get("Y").get
      case ("C", "Z") => drawScore + values.get("Z").get
    }
  }

  override def part1(lines: List[String]): String = {
    lines
      .foldLeft(0) { (score, line) =>
        val parts = line.split(" ")
        val scoreDiff = getScoreDiff(parts(0), parts(1))
        score + scoreDiff
      }
      .toString
  }

  override def part2(lines: List[String]): String = {
    val winningMoves = Map("A" -> "Y", "B" -> "Z", "C" -> "X")
    val drawingMoves = Map("A" -> "X", "B" -> "Y", "C" -> "Z")
    val losingMoves = Map("A" -> "Z", "B" -> "X", "C" -> "Y")
    val nextMoveMap = Map(
      "X" -> losingMoves,
      "Y" -> drawingMoves,
      "Z" -> winningMoves
    )
    lines
      .foldLeft(0) { (score, line) =>
        val parts = line.split(" ")
        val playA = parts(0)
        val playB = parts(1)
        val nextMove = nextMoveMap.get(playB).get.get(playA).get
        val scoreDiff = getScoreDiff(playA, nextMove)
        score + scoreDiff
      }
      .toString
  }
}
