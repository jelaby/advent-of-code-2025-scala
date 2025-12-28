package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike

object Day4 {

  @main
  def runDay4() = {

    println(part1(scala.io.Source.fromResource("day4-part1.txt").getLines()))
    println(part2(scala.io.Source.fromResource("day4-part1.txt").getLines()))

  }


  def at[T](m: Int => Option[Int => Option[T]], x: Int, y: Int, d: T): T = {
    m(y).flatMap(_(x)).getOrElse(d)
  }

  def part1(lines: Iterator[String]): Long = {

    val NEIGHBOURS = List((0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1))

    val present = lines.map(line => line.map(c => c == '@').toList).toList
    val lifted = present.map(l => l.lift).lift

    present.indices.flatMap(y => present(y).indices.map(x =>
        if present(y)(x) then
          NEIGHBOURS
            .map((dx, dy) => at(lifted, x + dx, y + dy, false))
            .count(b => b)
        else 9999))
      .count(n => n < 4)
  }

  def part2(lines: Iterator[String]): Long = {

    val NEIGHBOURS = List((0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1))

    val present = lines.map(line => line.map(c => c == '@').toList).toList

    def countRemovable(present : List[List[Boolean]]) : Int = {
      val lifted = present.map(l => l.lift).lift

      val toRemove = present.indices.map(y => present(y).indices.map(x =>
          if present(y)(x) then
            NEIGHBOURS
              .map((dx, dy) => at(lifted, x + dx, y + dy, false))
              .count(b => b)
          else 9999)
        .map(_ < 4))

      if toRemove.flatten.count(b => b) == 0 then
        0
      else
        toRemove.flatten.count(b => b) + countRemovable(
          present.zip(toRemove).map((l,rl) => l.zip(rl).map((p, r) => p && !r))
        )

    }

    countRemovable(present)

  }

  def peek[T](x: T): T = {
    println(x);
    x
  }


}

class Day4Test extends AnyFunSuiteLike {

  test("day 4 part 1") {
    assert(Day4.part1(
      """..@@.@@@@.
        |@@@.@.@.@@
        |@@@@@.@.@@
        |@.@@@@..@.
        |@@.@@@@.@@
        |.@@@@@@@.@
        |.@.@.@.@@@
        |@.@@@.@@@@
        |.@@@@@@@@.
        |@.@.@@@.@.""".stripMargin.linesIterator)
      === 13)
  }

  test("day 4 part 2") {
    assert(Day4.part2(
      """..@@.@@@@.
        |@@@.@.@.@@
        |@@@@@.@.@@
        |@.@@@@..@.
        |@@.@@@@.@@
        |.@@@@@@@.@
        |.@.@.@.@@@
        |@.@@@.@@@@
        |.@@@@@@@@.
        |@.@.@@@.@.""".stripMargin.linesIterator)
      === 43)
  }

}
