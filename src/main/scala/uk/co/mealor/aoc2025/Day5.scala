package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike

object Day5 {

  @main
  def runDay5() = {

    println(part1(scala.io.Source.fromResource("day5-part1.txt").getLines()))
    println(part2(scala.io.Source.fromResource("day5-part1.txt").getLines()))

  }


  def at[T](m: Int => Option[Int => Option[T]], x: Int, y: Int, d: T): T = {
    m(y).flatMap(_(x)).getOrElse(d)
  }

  private def toRangeList(lines: List[(Long, Long)]): List[(Long, Long)] = {
    toRangeList(lines.head, lines.tail, List(), ())
  }

  private def toRangeList(head: (Long, Long), tail: List[(Long, Long)], result: List[(Long, Long)], lastResult: (Long, Long) | Unit): List[(Long, Long)] = {

    def decide(tail: List[(Long, Long)], result: List[(Long, Long)], lastResult: (Long, Long)): List[(Long, Long)] = {
      if tail.isEmpty then {
        lastResult :: result
      } else {
        toRangeList(tail.head, tail.tail, result, lastResult)
      }
    }

    lastResult match
      case () => toRangeList(tail.head, tail.tail, result, head)
      case lr: (Long, Long) => if head(0) <= lr(1) + 1 then {
        decide(tail, result, (head(0).min(lr(0)), head(1).max(lr(1))))
      } else {
        decide(tail, lr :: result, head)
      }
  }

  private def isInRanges(v: Long, ranges: List[(Long, Long)]): Boolean = ranges.exists((s, e) => s <= v && e >= v)

  def part1(lineIter: Iterator[String]): Long = {

    val lines = lineIter.toList
    val (freshLines, ingredientLines) = lines.splitAt(lines.indexOf(""))

    val fresh = toRangeList(freshLines
      .map(_.split("-")
        .map(_.toLong))
      .collect { case Array(x, y) => (x, y) }
      .sortBy((s, _) => s))

    val ingredients = ingredientLines.tail.map(_.toLong)

    ingredients.count(isInRanges(_, fresh))
  }

  def part2(lineIter: Iterator[String]): Long = {

    val lines = lineIter.toList
    val (freshLines, ingredientLines) = lines.splitAt(lines.indexOf(""))

    toRangeList(freshLines
      .map(_.split("-")
        .map(_.toLong))
      .collect { case Array(x, y) => (x, y) }
      .sortBy((s, _) => s))
      .map((s,e) => e+1-s)
      .sum
  }

  def peek[T](x: T): T = {
    println(x);
    x
  }


}

class Day5Test extends AnyFunSuiteLike {

  test("day 5 part 1") {
    assert(Day5.part1(
      """3-5
        |10-14
        |16-20
        |12-18
        |
        |1
        |5
        |8
        |11
        |17
        |32""".stripMargin.linesIterator)
      === 3)
  }

  test("day 5 part 1 pdm 1") {
    assert(Day5.part1(
      """5-8
        |6-9
        |
        |1
        |2
        |3
        |4
        |5
        |6
        |7
        |8
        |9
        |10
        |11
        |12
        |13
        |14
        |15""".stripMargin.linesIterator)
      === 5)
  }

  test("day 5 part 1 pdm 2") {
    assert(Day5.part1(
      """7-8
        |9-10
        |
        |1
        |2
        |3
        |4
        |5
        |6
        |7
        |8
        |9
        |10
        |11
        |12
        |13
        |14
        |15""".stripMargin.linesIterator)
      === 4)
  }

  test("day 5 part 1 pdm 3") {
    assert(Day5.part1(
      """5-8
        |5-10
        |
        |1
        |2
        |3
        |4
        |5
        |6
        |7
        |8
        |9
        |10
        |11
        |12
        |13
        |14
        |15""".stripMargin.linesIterator)
      === 6)
  }

  test("day 5 part 1 pdm 4") {
    assert(Day5.part1(
      """5-10
        |6-9
        |
        |1
        |2
        |3
        |4
        |5
        |6
        |7
        |8
        |9
        |10
        |11
        |12
        |13
        |14
        |15""".stripMargin.linesIterator)
      === 6)
  }

  test("day 5 part 2") {
    assert(Day5.part2(
      """3-5
        |10-14
        |16-20
        |12-18
        |
        |1
        |5
        |8
        |11
        |17
        |32""".stripMargin.linesIterator)
      === 14)
  }

}
