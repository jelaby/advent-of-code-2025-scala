package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike
import os.{Path, RelPath}

import java.util.concurrent.ForkJoinPool
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ForkJoinTaskSupport

object Day10 {

  @main
  def runDay10() = {

    //println(part1(scala.io.Source.fromResource("day10.txt").getLines()))
    println(part2(scala.io.Source.fromResource("day10.txt").getLines(), "main"))

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

  def startingPoint(buttons: List[List[Int]], target: List[Int], buttonIndex: Int): (List[Int], Int) = {
    val button = buttons(buttonIndex)
    val lowest = button.zipWithIndex.filter((b, i) => b > 0).map((b, i) => target(i)).min
    val nextTarget = target.iterator.zip(button).map((t, b) => t - (b * lowest)).toList
    (nextTarget, lowest)
  }

  @tailrec
  def solve2Impl(name: String, target: List[Int], buttons: List[List[Int]], buttonCountHead: Int, buttonCount: List[Int], n: Long, startTime: Long): Int = {

    if n % 10000000 == 0 then {
      val now = System.currentTimeMillis()
      println(s"$name: \t\t\t$target ${buttonCount} ${n} in ${now - startTime}ms (${if n == 0 then 0 else ((now - startTime) * 1000000) / n} ps/unit)")
      savePart2Progress(name, target, buttonCountHead, buttonCount)
    }

    val buttonIndex = buttonCount.size + 1

    if !target.exists(_ > 0) then
      savePart2Progress(name, target, buttonCountHead, buttonCount)
      buttonCount.sum + buttonCountHead
    else if buttonIndex >= buttons.size then {
      val nextButtonsCount = buttonCount.dropWhile(_ == 0)
      val lastButton = buttons(buttonIndex - 1)
      val prevButton = buttons(nextButtonsCount.size - 1)
      val nextTarget = target.iterator.zip(prevButton).zip(lastButton)
        .map({ case ((a, b), c) => (a, b, c) }).map((t, b1, b2) => t + b1 + b2 * buttonCountHead).toList
      solve2Impl(name, nextTarget, buttons, nextButtonsCount.head - 1, nextButtonsCount.tail, n + 1, startTime)
    } else {
      val (nextTarget, lowest) = startingPoint(buttons, target, buttonIndex)
      solve2Impl(name, nextTarget, buttons, lowest, buttonCountHead :: buttonCount, n + 1, startTime)
    }
  }

  def solve2(name: String, target: List[Int], buttons: List[BitSet]): Long = {

    println(s"$name: \tSTART\t{${target.mkString(",")}}\t${buttons.map(_.mkString(",")).mkString("(", "),(", ")")}")

    val result = loadPart2(target, buttons) match {
      case Some(r) => r
      case _ => {

        val sortedButtons = buttons
          .sortBy(button => (button.size * buttons.size) - buttons.map(other => button.intersect(other).size).sum)
          .sortBy(-_.size)
          //.sortBy(button => -buttons.foldLeft(button)((a, b) => a.intersect(b)).size)
          .map(button => target.indices.map(i => if button.contains(i) then 1 else 0).toList)

        val r = loadPart2Progress(name) match {
          case Some((loadedTarget, loadedButtonCountHead, loadedButtonCount)) => {
            solve2Impl(name, loadedTarget, sortedButtons, loadedButtonCountHead, loadedButtonCount, 0, System.currentTimeMillis())
          }
          case None => {
            val (nextTarget, lowest) = startingPoint(sortedButtons, target, 0)
            solve2Impl(name, nextTarget, sortedButtons, lowest, List(), 0, System.currentTimeMillis())
          }
        }
        savePart2(target, buttons, r)
        r
      }
    }

    println(s"$name: \tDONE\t${result}")

    result
  }

  def part2Cache(target: List[Int], buttons: List[BitSet]): Path = {
    val path = os.pwd / "cache" / "day10" / s"${buttons.map(_.mkString(",")).mkString("-")}_${target.mkString(",")}"
    os.makeDir.all(path / RelPath.up)
    path
  }

  def loadPart2(target: List[Int], buttons: List[BitSet]): Option[Int] = {
    val file = part2Cache(target, buttons)
    if os.exists(file) then
      println(s"Loading ${file}")
      Some(os.read(file)).flatMap(_.toIntOption)
    else
      println(s"No cache file ${file}")
      None
  }

  def savePart2(target: List[Int], buttons: List[BitSet], result: Int): Unit = {
    val file = part2Cache(target, buttons)
    if !os.exists(file) then {
      os.write(file, result.toString)
      println(s"Wrote ${file}")
    } else
      println(s"Skipped writing ${file}: already exists")

  }

  def part2ProgressFile(name: String): Path = {
    val path = os.pwd / "cache" / "day10-progress" / name
    os.makeDir.all(path / RelPath.up)
    path
  }

  def loadPart2Progress(name: String): Option[(List[Int], Int, List[Int])] = {
    val file = part2ProgressFile(name)
    if os.exists(file) then
      println(s"Loading ${file}")
      os.read(file).split("/") match {
        case Array(target, buttonCountHead, buttonCount) =>
          Some((
            target.split(",").map(_.toInt).toList,
            buttonCountHead.toInt,
            buttonCount.split(",").map(_.toInt).toList
          ))
        case _ => None
      }
    else
      println(s"No progress file ${file}")
      None
  }

  def savePart2Progress(name: String, target: List[Int], buttonCountHead: Int, buttonCount: List[Int]): Unit = {
    val file = part2ProgressFile(name)
    os.write.over(file, s"${target.mkString(",")}/${buttonCountHead}/${buttonCount.mkString(",")}")
    println(s"Wrote ${file}")
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

  def part2(lines: Iterator[String], name: String): Long = {

    val par = lines.map(parse)
      .zipWithIndex.map((items, i) => {
        val (_, buttons, target) = items;
        (i, buttons, target)
      })
      .toList
      .par
    par.tasksupport = new ForkJoinTaskSupport(ForkJoinPool(Runtime.getRuntime.availableProcessors()))
    par
      .map((i, buttons, target) => solve2(s"$name $i", target, buttons))
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
        |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin.linesIterator,
      "day 10 part 2")
      === 33)
  }

  test("day 10 part 2 eg 1") {
    assert(Day10.part2(
      """[...##] (0,1) (1,2) (2,3) (3,4) {1,2,2,2,1}""".stripMargin.linesIterator,
      "day 10 part 2 eg 1")
      === 4)
  }

  test("day 10 part 2 eg 2") {
    assert(Day10.part2(
      """[...##] (0) (0,1) (0,1,2) (0,1,2,3) {1,0,0,0}""".stripMargin.linesIterator,
      "day 10 part 2 eg 2")
      === 1)
  }

  test("day 10 part 2 eg 3") {
    assert(Day10.part2(
      """[...##] (0) (0,1) (0,1,2) (0,1,2,3) {2,2,1,1}""".stripMargin.linesIterator,
      "day 10 part 2 eg 3")
      === 2)
  }
}
