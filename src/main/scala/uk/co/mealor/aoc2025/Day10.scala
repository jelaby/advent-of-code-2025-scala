package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike
import os.Path

import java.util.concurrent.{Executors, ForkJoinPool}
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ForkJoinTaskSupport

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


  @tailrec
  def solve2Impl(row: Int, target: List[Int], buttons: List[List[Int]], buttonCountHead: Int, buttonCount: List[Int], n: Int): Int = {

    if n % 10000000 == 0 then {
      println(s"$row: \t\t\t$target $buttonCountHead ${buttonCount}")
    }

    val buttonIndex = buttonCount.size

    if !target.exists(_ > 0) then

      buttonCount.sum + (buttonCountHead max 0)
    else if buttonIndex >= buttons.size then {
      val nextButtonsCount = buttonCount.dropWhile(_ == 0)
      val prevButton = buttons(nextButtonsCount.size - 1)
      val nextTarget = target.iterator.zip(prevButton).map((t, b) => t + b).toList
      solve2Impl(row, nextTarget, buttons, -1, (nextButtonsCount.head - 1) :: nextButtonsCount.tail, n + 1)
    } else if buttonCountHead < 0 then {
      val button = buttons(buttonIndex)
      val lowest = button.zipWithIndex.filter((b, i) => b > 0).map((b, i) => target(i)).min
      val nextTarget = target.iterator.zip(button).map((t, b) => t - (b * lowest)).toList
      solve2Impl(row, nextTarget, buttons, -1, lowest :: buttonCount, n + 1)

    } else {
      val button = buttons(buttonIndex)
      val nextTarget = target.iterator.zip(button).map((t, b) => t - b).toList

      if nextTarget.exists(_ < 0) then
        solve2Impl(row, target, buttons, 0, buttonCountHead :: buttonCount, n + 1)
      else
        solve2Impl(row, nextTarget, buttons, buttonCountHead + 1, buttonCount, n + 1)
    }
  }

  def solve2(row: Int, target: List[Int], buttons: List[BitSet]): Long = {

    println(s"$row: \tSTART\t{${target.mkString(",")}}\t${buttons.map(_.mkString(",")).mkString("(", "),(", ")")}")

    val result = loadPart2(target, buttons) match {
      case Some(r) => r
      case _ => {

        val r = solve2Impl(row, target, buttons
          .sortBy(button => (button.size * buttons.size) - buttons.map(other => button.intersect(other).size).sum)
          .sortBy(-_.size)
          //.sortBy(button => -buttons.foldLeft(button)((a, b) => a.intersect(b)).size)
          .map(button => target.indices.map(i => if button.contains(i) then 1 else 0).toList), -1, List(), 0)
        savePart2(target, buttons, r)
        r
      }
    }

    println(s"$row: \tDONE\t${result}")

    result
  }

  def part2Cache(target: List[Int], buttons: List[BitSet]): Path = {
    os.pwd / "cache" / "day10" / s"${buttons.map(_.mkString(",")).mkString("-")}_${target.mkString(",")}"
  }

  def loadPart2(target: List[Int], buttons: List[BitSet]): Option[Int] = {
    val file = part2Cache(target, buttons)
    if os.exists(file) then
      Some(os.read(file)).flatMap(_.toIntOption)
    else
      None
  }

  def savePart2(target: List[Int], buttons: List[BitSet], result: Int): Unit = {
    def mkdirs(p: Path): Unit = {
      if !os.isDir(p) then {
        mkdirs(p / "..")
        os.makeDir(p)
      }
    }

    val file = part2Cache(target, buttons)
    mkdirs(file / "..")
    os.write(file, result.toString)
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

    val par = lines.map(parse)
      .zipWithIndex.map((items, i) => {
        val (_, buttons, target) = items;
        (i, buttons, target)
      })
      .toList
      .par
    par.tasksupport = new ForkJoinTaskSupport(ForkJoinPool(Runtime.getRuntime.availableProcessors()/2))
    par
      .map((i, buttons, target) => solve2(i, target, buttons))
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
