package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike
import scalaz.Memo

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, SortedSet}

object Day10 {

  @main
  def runDay10() = {

    println(part1(scala.io.Source.fromResource("day10.txt").getLines()))
    println(part2(scala.io.Source.fromResource("day10.txt").getLines()))

  }

  def parse(line: String): (BitSet, List[BitSet]) = {

    val parts = line.split(" ").toList

    val target = parts.head.substring(1, parts.head.length - 1)
      .iterator
      .zipWithIndex
      .flatMap((c, i) => c match {
        case '.' => Iterator()
        case '#' => Iterator(i)
      })
      .to(BitSet)

    val (buttonStrings, voltageStrings) = parts.tail.partition(_(0) == '(')

    val buttons = buttonStrings.iterator
      .map(s => s.substring(1, s.length - 1))
      .map(_.split(",").map(_.toInt).to(BitSet))
      .toList

    (target, buttons)
  }

  def solve(target: BitSet, buttons: List[BitSet]): Long = {
    solve(target, buttons.toSet, BitSet.empty, Integer.MAX_VALUE)
  }

  def solve(target: BitSet, buttons: Set[BitSet], current: BitSet, bestSoFar: Long): Long = {

    if current == target then
      0
    else if buttons.isEmpty || bestSoFar == 0 then
      Integer.MAX_VALUE
    else {
      1 + buttons.iterator
        .foldLeft(Integer.MAX_VALUE.toLong)((best, button) => best min solve(target, buttons - button, current ^ button, (bestSoFar min best) - 1))
    }
  }

  def part1(lines: Iterator[String]): Long = {

    lines.map(parse)
      .zipWithIndex.map((l,i) => peek((i,l))).map((i,l) => l)
      .map((target, buttons) => solve(target, buttons)).sum

  }

  def part2(lines: Iterator[String]): Long = {
    0
  }

  def peek[T](x: T): T = {
    println(x);
    x
  }


}

class Day10Test extends AnyFunSuiteLike {

  test("day 10 part 1") {
    assert(Day10.part1(
      """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
        |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
        |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin.linesIterator)
      === 7)
  }

  test("day 10 example") {
    assert(Day10.part1(
      """[...##] (1,2) (1,3) (0,1,3) (0,2,4) (0,1,3,4) {35,47,29,29,23}""".stripMargin.linesIterator)
      === 3)
  }


  test("day 10 part 2") {
    assert(Day10.part2(
      """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
        |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
        |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin.linesIterator)
      === 24)
  }
}
