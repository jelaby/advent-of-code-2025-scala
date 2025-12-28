package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike
import scalaz.Memo

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object Day9 {

  @main
  def runDay9() = {

    println(part1(scala.io.Source.fromResource("day9.txt").getLines()))
    println(part2(scala.io.Source.fromResource("day9.txt").getLines()))

  }


  def part1(lines: Iterator[String]): Long = {

    val redTiles = lines.map(line => line.split(",").map(_.toLong) match {case Array(x,y) => (x,y)})
      .toList.to(SortedSet)

    redTiles.flatMap(a => redTiles.map(b => ((b(0)-a(0)).abs+1) * ((b(1)-a(1)).abs + 1))).max
  }

  def part2(lines: Iterator[String]): Long = {
0
  }

  def peek[T](x: T): T = {
    println(x);
    x
  }


}

class Day9Test extends AnyFunSuiteLike {

  test("day 9 part 1") {
    assert(Day9.part1(
      """7,1
        |11,1
        |11,7
        |9,7
        |9,5
        |2,5
        |2,3
        |7,3""".stripMargin.linesIterator)
      === 50)
  }


  test("day 9 part 2") {
    assert(Day9.part2(
      """7,1
        |11,1
        |11,7
        |9,7
        |9,5
        |2,5
        |2,3
        |7,3""".stripMargin.linesIterator)
      === 25272)
  }

}
