package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike
import scalaz.Memo

import scala.annotation.tailrec

object Day7 {

  @main
  def runDay7() = {

    println(part1(scala.io.Source.fromResource("day7-part1.txt").getLines()))
    println(part2(scala.io.Source.fromResource("day7-part1.txt").getLines()))

  }


  def at[T](m: Int => Option[Int => Option[T]], x: Int, y: Int, d: T): T = {
    m(y).flatMap(_(x)).getOrElse(d)
  }

  def findChars(line: String, toFind: Char): Set[Int] = {
    Set.from(line.zipWithIndex.filter((c, i) => c == toFind).map((c, i) => i))
  }

  def initialBeam(line: String): Set[Int] = {
    findChars(line, 'S')
  }

  def findSplitters(line: String): Set[Int] = {
    findChars(line, '^')
  }

  @tailrec
  def propagate(splits: Int, beams: Set[Int], lines: Iterator[String]): (Int, Set[Int]) = {
    if lines.isEmpty then
      (splits, beams)
    else {

      val splitters = findSplitters(lines.next())

      val newSplits = splitters.count(beams.contains)
      val newBeams = beams.flatMap(ib => if splitters.contains(ib) then Iterable(ib - 1, ib + 1) else Iterable(ib))

      (0 until newBeams.max.max(splitters.maxOption.getOrElse(0)) + 1)
        .foreach(i => print(if splitters.contains(i) && newBeams.contains(i) then {
          "X"
        } else if splitters.contains(i) then {
          "^"
        } else if newBeams.contains(i) then {
          "|"
        } else {
          "."
        }))
      println(" " + newSplits)

      propagate(splits + newSplits, newBeams, lines)
    }
  }

  val quantumPropagate: ((Int, List[String])) => Long = Memo.mutableHashMapMemo((beam: Int, lines: List[String]) => {
    if lines.isEmpty then
      1
    else {

      val splitters = findSplitters(lines.head)

      if splitters.contains(beam) then
        quantumPropagate(beam - 1, lines.tail) + quantumPropagate(beam + 1, lines.tail)
      else
        quantumPropagate(beam, lines.tail)
    }
  })

  def part1(lines: Iterator[String]): Long = {

    val beams = initialBeam(lines.next())

    propagate(0, beams, lines)(0)
  }

  def part2(lineIter: Iterator[String]): Long = {
    val lines = lineIter.toList
    val beams = initialBeam(lines.head)
    quantumPropagate((beams.head, lines))
  }

  def peek[T](x: T): T = {
    println(x);
    x
  }


}

class Day7Test extends AnyFunSuiteLike {

  test("day 7 part 1") {
    assert(Day7.part1(
      """.......S.......
        |...............
        |.......^.......
        |...............
        |......^.^......
        |...............
        |.....^.^.^.....
        |...............
        |....^.^...^....
        |...............
        |...^.^...^.^...
        |...............
        |..^...^.....^..
        |...............
        |.^.^.^.^.^...^.
        |...............""".stripMargin.linesIterator)
      === 21)
  }

  test("day 7 part 1 a") {
    assert(Day7.part1(
      """.......S.......""".stripMargin.linesIterator)
      === 0)
  }

  test("day 7 part 1 b") {
    assert(Day7.part1(
      """.......S.......
        |...............""".stripMargin.linesIterator)
      === 0)
  }

  test("day 7 part 1 c") {
    assert(Day7.part1(
      """.......S.......
        |...............
        |.......^.......""".stripMargin.linesIterator)
      === 1)
  }

  test("day 7 part 1 d") {
    assert(Day7.part1(
      """.......S.......
        |...............
        |.......^.......
        |...............""".stripMargin.linesIterator)
      === 1)
  }

  test("day 7 part 1 e") {
    assert(Day7.part1(
      """.......S.......
        |...............
        |.......^.......
        |...............""".stripMargin.linesIterator)
      === 1)
  }

  test("day 7 part 1 f") {
    assert(Day7.part1(
      """.......S.......
        |...............
        |.......^.......
        |...............
        |......^.^......""".stripMargin.linesIterator)
      === 3)
  }

  test("day 7 part 2") {
    assert(Day7.part2(
      """.......S.......
        |...............
        |.......^.......
        |...............
        |......^.^......
        |...............
        |.....^.^.^.....
        |...............
        |....^.^...^....
        |...............
        |...^.^...^.^...
        |...............
        |..^...^.....^..
        |...............
        |.^.^.^.^.^...^.
        |...............""".stripMargin.linesIterator)
      === 40)
  }

  test("day 7 part 2 a") {
    assert(Day7.part2(
      """.......S.......
        |...............""".stripMargin.linesIterator)
      === 1)
  }

  test("day 7 part 2 b") {
    assert(Day7.part2(
      """.......S.......
        |...............
        |.......^.......""".stripMargin.linesIterator)
      === 2)
  }

  test("day 7 part 2 c") {
    assert(Day7.part2(
      """.......S.......
        |.......^.......
        |......^.^......""".stripMargin.linesIterator)
      === 4)
  }

  test("day 7 part 2 d") {
    assert(Day7.part2(
      """.......S.......
        |.......^.......
        |......^........""".stripMargin.linesIterator)
      === 3)
  }

  test("day 7 part 2 e") {
    assert(Day7.part2(
      """.......S.......
        |.......^.......
        |...............""".stripMargin.linesIterator)
      === 2)
  }

}
