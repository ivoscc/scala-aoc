package to.ivo.aoc.y2022

import to.ivo.aoc.Day

object Day08 extends Day {
  override def part1(lines: List[String]): String = {
    val grid = parseInput(lines)
    var counter = 0
    for {
      row <- 0 to grid.length - 1
      col <- 0 to grid(0).length - 1
    } {
      if (checkVisible(grid, row, col)) {
        counter = counter + 1
      }
    }
    counter.toString // 1851
  }

  override def part2(lines: List[String]): String = {
    val grid = parseInput(lines)
    var maxSoFar = 0
    for {
      row <- 0 to grid.length - 1
      col <- 0 to grid(0).length - 1
    } {
      val viewingDistance = countViewingDistance(grid, row, col)
      if (viewingDistance > maxSoFar) {
        maxSoFar = viewingDistance
      }
    }
    maxSoFar.toString // 574080
  }

  private def parseInput(lines: List[String]): Array[Array[Int]] =
    lines.map(_.map(_.toString.toInt).toArray).toArray

  def checkVisible(grid: Array[Array[Int]], initialRow: Int, initialCol: Int): Boolean = {
    val currentValue = grid(initialRow)(initialCol)

    val leftSide = grid(initialRow).take(initialCol)
    val rightSide = grid(initialRow).drop(initialCol + 1)
    val topSide = grid.map(_(initialCol)).take(initialRow)
    val bottomSide = grid.map(_(initialCol)).drop(initialRow + 1)

    val isVisible: Int => Boolean = _ < currentValue
    (leftSide.forall(isVisible)
    || rightSide.forall(isVisible)
    || topSide.forall(isVisible)
    || bottomSide.forall(isVisible))
  }

  def countViewingDistance(grid: Array[Array[Int]], initialRow: Int, initialCol: Int): Int = {
    val currentValue = grid(initialRow)(initialCol)

    val leftSide = grid(initialRow).take(initialCol).reverse.toList
    val rightSide = grid(initialRow).drop(initialCol + 1).toList
    val topSide = grid.map(_(initialCol)).take(initialRow).reverse.toList
    val bottomSide = grid.map(_(initialCol)).drop(initialRow + 1).toList

    (countViewDistance(leftSide, currentValue)
      * countViewDistance(rightSide, currentValue)
      * countViewDistance(topSide, currentValue)
      * countViewDistance(bottomSide, currentValue))
  }

  private def countViewDistance(trees: List[Int], currentValue: Int): Int = {
    var counter = 0
    for (i <- 0 to trees.length - 1) {
      if (trees(i) >= currentValue) {
        return counter + 1
      }
      counter += 1
    }
    counter.max(1)
  }

}
