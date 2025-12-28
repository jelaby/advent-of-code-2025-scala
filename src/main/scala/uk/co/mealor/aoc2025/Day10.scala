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

  def startingPoint(buttons: List[List[Int]], target: List[Int], buttonIndex: Int): (MutatingList, Int) = {
    startingPoint(buttons, MutatingList(target, target.length), buttonIndex)
  }

  def startingPoint(buttons: List[List[Int]], target: MutatingList, buttonIndex: Int): (MutatingList, Int) = {
    val button = buttons(buttonIndex)
    val lowest = button.zipWithIndex.filter((b, i) => b > 0).map((b, i) => target(i)).min

    target + button.map(-_ * lowest)

    (target, lowest)
  }

  class MutatingList(val content: Array[Int], var offset: Int) extends IterableOnce[Int] {

    def this(maxSize: Int) = {
      this(new Array[Int](maxSize), maxSize)
    }

    def this(other: List[Int], maxSize: Int) = {
      this(maxSize)
      other.reverseIterator.foreach(_ :: this)
    }

    def apply(index: Int): Int = {
      content(offset + index)
    }

    def +(other: IterableOnce[Int]): MutatingList = {
      other.iterator.zipWithIndex.foreach((o, i) => content(offset + i) += o)
      this
    }

    def ::(other: Int): MutatingList = {
      offset -= 1
      content(offset) = other
      this
    }

    private def actualIndices: Range = (offset until content.length)

    def exists(test: Int => Boolean): Boolean = {
      actualIndices.exists(i => test(content(i)))
    }

    def size: Int = content.length - offset

    def sum: Int = actualIndices.map(i => content(i)).sum

    def dropWhile(test: Int => Boolean): MutatingList = {
      while (offset < content.length && test(content(offset)))
        offset += 1
      this
    }

    def head: Int = content(offset)

    def tail: MutatingList = {
      offset += 1
      this
    }

    override def toString(): String = {
      actualIndices.map(content(_)).mkString("MutatingList(", ",", s"; $offset/${content.length})")
    }

    def toList: List[Int] = {
      List.from(this)
    }

    override def iterator: Iterator[Int] = actualIndices.iterator.map(content(_))
  }

  @tailrec
  def solve2Impl(name: String, target: MutatingList, buttons: List[List[Int]], buttonCount: MutatingList, n: Long, startTime: Long): Int = {

    if n % 10000000 == 0 then {
      val now = System.currentTimeMillis()
      println(s"$name: \t\t\t$target ${buttonCount} ${n} in ${now - startTime}ms (${if n == 0 then 0 else ((now - startTime) * 1000000) / n} ps/unit)")
      savePart2Progress(name, target, buttonCount)
    }

    val buttonIndex = buttonCount.size

    if !target.exists(_ > 0) then {
      savePart2Progress(name, target, buttonCount)
      buttonCount.sum
    } else if buttonIndex >= buttons.size then {
      val buttonsCountHead = buttonCount.head

      buttonCount.tail.dropWhile(_ == 0)

      val nextButtonsCountHead = buttonCount.head


      val lastButton = buttons(buttonIndex - 1)
      val prevButton = buttons(buttonCount.size - 1)
      target + lastButton.iterator.map(_ * buttonsCountHead).zip(prevButton).map(_ + _)
      (nextButtonsCountHead - 1) :: (buttonCount.tail)
      solve2Impl(name, target, buttons, buttonCount, n + 1, startTime)
    } else {
      val (nextTarget, lowest) = startingPoint(buttons, target, buttonIndex)
      solve2Impl(name, nextTarget, buttons, lowest :: buttonCount, n + 1, startTime)
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
            solve2Impl(name, MutatingList(loadedTarget, loadedTarget.length), sortedButtons, MutatingList(loadedButtonCountHead :: loadedButtonCount, buttons.length), 0, System.currentTimeMillis())
          }
          case None => {
            val (nextTarget, lowest) = startingPoint(sortedButtons, target, 0)
            solve2Impl(name, nextTarget, sortedButtons, MutatingList(List(lowest), buttons.length), 0, System.currentTimeMillis())
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

  def savePart2Progress(name: String, target: MutatingList, buttonCount: MutatingList): Unit = {
    savePart2Progress(name, target.toList, buttonCount.head, buttonCount.toList.tail)
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

  test("MutatingList apply") {
    val l = Day10.MutatingList(List(11, 22, 33), 10)

    assert(l(0) === 11)
    assert(l(1) === 22)
    assert(l(2) === 33)
  }

  test("MutatingList prepend") {
    val l = Day10.MutatingList(List(11, 22, 33), 10)
    val l2 = 99 :: l

    assert(l === l2)

    assert(l(0) === 99)
    assert(l(1) === 11)
    assert(l(2) === 22)
    assert(l(3) === 33)
  }

  test("MutatingList tail") {
    val l = Day10.MutatingList(List(11, 22, 33), 10)
    val l2 = l.tail
    assert(l === l2)
    assert(l(0) === 22)
    assert(l(1) === 33)
  }

  test("MutatingList +") {
    val l = Day10.MutatingList(List(11, 22, 33), 10)
    val other = List(3, 5, 7);
    val l2 = (l + other)
    assert(l === l2)
    assert(l(0) === 14)
    assert(l(1) === 27)
    assert(l(2) === 40)
  }
}
