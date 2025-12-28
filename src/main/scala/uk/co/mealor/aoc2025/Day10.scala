package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike
import scalaz.Memo

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, SortedSet}

object Day10 {

  @main
  def runDay10() = {

    //println(part1(scala.io.Source.fromResource("day10.txt").getLines()))
    println(part2(scala.io.Source.fromResource("day10.txt").getLines()))

  }

  def parse(line: String): (BitSet, List[BitSet], List[Int]) = {

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

    val voltages = voltageStrings.iterator
      .map(s => s.substring(1, s.length - 1))
      .map(_.split(",").map(_.toInt).toList)
      .next()

    (target, buttons, voltages)
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


  def solve2(target: List[Int], buttons: List[BitSet]): Long = {

    @tailrec
    def solve2Impl(target: List[Int], targetStack: List[List[Int]], buttons: List[List[Int]], buttonIndex: Int, buttonIndexStack: List[Int], n: Int): Int = {

      if (n == 0) then {
        println(s"$target\t${buttons.map(button => button.count(_ == 1))}")
      } else if n % 10000000 == 0 then {
        println(s"$target\t$buttonIndex\t${buttons.indices.map(i => buttonIndexStack.count(_ == i))}")
      }

      if buttonIndex >= buttons.size then {
        solve2Impl(targetStack.head, targetStack.tail, buttons, buttonIndexStack.head + 1, buttonIndexStack.tail, n + 1)
      } else if !target.exists(_ > 0) then

        targetStack.size

      else {
        val button = buttons(buttonIndex)
        val nextTarget = target.iterator.zip(button).map((t, b) => t - b).toList

        if nextTarget.exists(_ < 0) then
          solve2Impl(target, targetStack, buttons, buttonIndex + 1, buttonIndexStack, n + 1)
        else
          solve2Impl(nextTarget, target :: targetStack, buttons, buttonIndex, buttonIndex :: buttonIndexStack, n + 1)
      }
    }

    solve2Impl(target,
      List(),
      buttons
        .sortBy(-_.size)
        .sortBy(button => -buttons.foldLeft(button)((a,b) => a.intersect(b)).size)
        .map(button => target.indices.map(i => if button.contains(i) then 1 else 0).toList),
      0,
      List(),
      0)
  }

  /*
  3,5,4,7
  3,4,4,6
  3,3,4,5
  3,3,3,4
  3,2,3,3
  3,2,2,2
  2,2,1,2
  1,1,1,1
  0,0,1,1
  0,0,0,0
   */
  /*
  7,5,12,7,2
  6,5,11,6,1
  5,5,10,5,0
  4,4,9,5,0
  4,4,8,4,0
  3,3,7,4,0
  3,3,6,3,0
  2,2,5,3
  2,2,4,2
  1,1,3,2
  1,1,2,1
  0,0,1,1
  0,0,0,0
   */

  def part1(lines: Iterator[String]): Long = {

    lines.map(parse)
      .zipWithIndex.map((l, i) => peek((i, l))).map((i, l) => l)
      .map((target, buttons, _) => solve(target, buttons)).sum

  }

  def part2(lines: Iterator[String]): Long = {
    lines.map(parse)
      .zipWithIndex.map((l, i) => peek((i, l))).map((i, l) => l)
      .map((_, buttons, target) => solve2(target, buttons))
      .map(peek)
      .sum
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

  test("day 10 part 2") {
    assert(Day10.part2(
      """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
        |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
        |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin.linesIterator)
      === 33)
  }
}
