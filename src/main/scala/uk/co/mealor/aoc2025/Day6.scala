package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike

object Day6 {

  @main
  def runDay6() = {

    println(part1(scala.io.Source.fromResource("day6-part1.txt").getLines()))
    println(part2(scala.io.Source.fromResource("day6-part1.txt").getLines()))

  }


  def at[T](m: Int => Option[Int => Option[T]], x: Int, y: Int, d: T): T = {
    m(y).flatMap(_(x)).getOrElse(d)
  }

  def toOp(op: String): (Long, (Long, Long) => Long) = {
    toOp(op.head)
  }

  def toOp(op: Char): (Long, (Long, Long) => Long) = {
    op match {
      case '+' => (0, (a: Long, b: Long) => a + b)
      case '*' => (1, (a: Long, b: Long) => a * b)
      case _ => throw new Exception("Unknown operator '" + op + "'")
    }
  }

  def part1(lineIter: Iterator[String]): Long = {

    val (operators, lines) = lineIter
      .map(_.trim).map(_.split("\\s+")).toList
      .partition(_.head.matches("^\\D"))


    operators.head
      .map(toOp)
      .zipWithIndex
      .map((op, i) => lines.map(_(i)).map(_.toLong).foldLeft(op(0))(op(1)))
      .sum

  }

  def safeSubstring(string: String, s: Int, e: Int): String = {
    string.substring(string.length.min(s), string.length.min(e))
  }

  def safeSubstring(string: String, s: Int): String = {
    string.substring(string.length.min(s), string.length)
  }

  def part2(lineIter: Iterator[String]): Long = {


    val (operatorLines, lines) = lineIter.toList.map(_ + " ").partition(line => Array('+', '*').contains(line.head))
    val operatorLine = operatorLines.head

    def previousOrMax(i: Int): Int = {
      i match
        case -1 => Int.MaxValue
        case other => other
    }

    def nextEnd(s: String): Int = {
      s.length.min(previousOrMax(s.indexOf('+', 1))
        .min(previousOrMax(s.indexOf('*', 1))))
    }

    def splitProblems(operatorLine: String, lines: List[String]): List[((Long, ((Long, Long) => Long)), List[Long])] = {
      if operatorLine.isEmpty then
        (List[((Long, ((Long, Long) => Long)), List[Long])]())
      else {

        val endIndex = nextEnd(operatorLine)

        val op = toOp(operatorLine.head)

        val strings = lines.map(safeSubstring(_, 0, endIndex - 1))

        val values = strings.head.indices
          .map(i => strings.map(_(i)).foldLeft(0L)((acc,c) => if c == ' ' then acc else acc * 10 + (c - '0')))
          .toList

        (op, values) :: splitProblems(safeSubstring(operatorLine, endIndex), lines.map(safeSubstring(_, endIndex)))
      }

    }

    splitProblems(operatorLine, lines)
      .map((op, values) => values.foldLeft(op(0))(op(1)))
      .sum

  }

  def peek[T](x: T): T = {
    println(x);
    x
  }


}

class Day6Test extends AnyFunSuiteLike {

  test("day 6 part 1") {
    assert(Day6.part1(
      """123 328  51 64\u0020
        | 45 64  387 23\u0020
        |  6 98  215 314
        |*   +   *   +  """.stripMargin.linesIterator)
      === 4277556)
  }

  test("day 6 part 1 +test") {
    assert(Day6.part1(
      """  2
        |  5
        |+  """.stripMargin.linesIterator)
      === 7)
  }

  test("day 6 part 1 *test") {
    assert(Day6.part1(
      """  2
        |  5
        |*  """.stripMargin.linesIterator)
      === 10)
  }

  test("day 6 part 2") {
    assert(Day6.part2(
      """123 328  51 64
        | 45 64  387 23
        |  6 98  215 314
        |*   +   *   +  """.stripMargin.linesIterator)
      === 3263827)
  }

}
