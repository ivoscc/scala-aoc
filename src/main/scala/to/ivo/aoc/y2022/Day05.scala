package to.ivo.aoc.y2022

import scala.collection.mutable.{ArrayBuffer, Stack}
import to.ivo.aoc.Day

object Day05 extends Day {

  case class Instruction(times: Int, source: Int, destination: Int)

  class Boxes(items: List[List[String]]) {
    private val stacks: ArrayBuffer[Stack[String]] = ArrayBuffer()

    for (line <- items) {
      addLine(line)
    }

    def addLine(line: List[String]) =
      for ((box, index) <- line.zipWithIndex) {
        if (stacks.length <= index) {
          stacks += Stack()
        }
        if (!box.isEmpty) {
          stacks(index).push(box)
        }
      }

    def moveBox(sourceIndex: Int, destinationIndex: Int) = {
      val moved = stacks(sourceIndex - 1).pop
      stacks(destinationIndex - 1).push(moved)
    }

    def multiMove(times: Int, sourceIndex: Int, destinationIndex: Int) = {
      val temp = Stack[String]()
      for (_ <- 1 to times) {
        temp.push(stacks(sourceIndex - 1).pop())
      }
      stacks(destinationIndex - 1).pushAll(temp)
    }

    def getTopBoxes(): String =
      stacks.map(stack => stack.reverse.last.charAt(1)).mkString("")

    override def toString(): String =
      stacks
        .map(stack => stack.reverse.mkString(" "))
        .toList
        .mkString("\n")
  }

  override def part1(lines: List[String]): String = {
    val (boxes, instructions) = parseInput(lines)
    for (instruction <- instructions) {
      for (i <- 1 to instruction.times) {
        boxes.moveBox(instruction.source, instruction.destination)
      }
    }

    boxes.getTopBoxes // TGWSMRBPN
  }

  override def part2(lines: List[String]): String = {
    val (boxes, instructions) = parseInput(lines)
    for (instruction <- instructions) {
      boxes.multiMove(instruction.times, instruction.source, instruction.destination)
    }
    boxes.getTopBoxes // TZLTLWRNF
  }

  private def parseInput(lines: List[String]): (Boxes, List[Instruction]) = {
    val (boxesInput, instructionsInput) = lines.span(!_.isEmpty)
    val boxes = new Boxes(
      boxesInput
        .map(parseBoxLevel)
        .reverse
        .drop(1)
        .foldLeft(List[List[String]]())((boxes, items) => boxes :+ items)
    )
    val instructions = instructionsInput.tail.map(parseInstruction)
    (boxes, instructions)
  }

  private def parseInstruction(instruction: String): Instruction = {
    instruction match {
      case s"move $times from $source to $destination" =>
        Instruction(times.toInt, source.toInt, destination.toInt)
    }
  }

  private def parseBoxLevel(level: String): List[String] = level.grouped(4).map(_.trim).toList

}
