package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike

object Day3 {

  @main
  def runDay3() = {

    println(part1(scala.io.Source.fromResource("day3-part1.txt").getLines()))
    println(part2(scala.io.Source.fromResource("day3-part1.txt").getLines()))

  }

  def part1(lines: Iterator[String]): Long = {

    lines
      .map(l => l.chars.map(_ - '0').toArray)
      .map(bank =>
        val i1 = (0 until bank.length - 1).maxBy(bank(_));
        val i2 = (i1 + 1 until bank.length).maxBy(bank(_))
        bank(i1) * 10 + bank(i2)
      )
      .sum

  }

  def part2(lines: Iterator[String]): Long = {

    lines
      .map(l => l.chars.map(_ - '0').toArray)
      .map(bank =>
        val i1 = (0 until bank.length - 11).maxBy(bank(_));
        val i2 = (i1 + 1 until bank.length - 10).maxBy(bank(_))
        val i3 = (i2 + 1 until bank.length - 9).maxBy(bank(_))
        val i4 = (i3 + 1 until bank.length - 8).maxBy(bank(_))
        val i5 = (i4 + 1 until bank.length - 7).maxBy(bank(_))
        val i6 = (i5 + 1 until bank.length - 6).maxBy(bank(_))
        val i7 = (i6 + 1 until bank.length - 5).maxBy(bank(_))
        val i8 = (i7 + 1 until bank.length - 4).maxBy(bank(_))
        val i9 = (i8 + 1 until bank.length - 3).maxBy(bank(_))
        val i10 = (i9 + 1 until bank.length - 2).maxBy(bank(_))
        val i11 = (i10 + 1 until bank.length - 1).maxBy(bank(_))
        val i12 = (i11 + 1 until bank.length).maxBy(bank(_))

        (bank(i1) * 100000000000L) + (bank(i2) * 10000000000L) + (bank(i3) * 1000000000L) + (bank(i4) * 100000000L) + (bank(i5) * 10000000L) + (bank(i6) * 1000000L) + (bank(i7) * 100000L) + (bank(i8) * 10000L) + (bank(i9) * 1000L) + (bank(i10) * 100L) + (bank(i11) * 10L) + (bank(i12))
      )
      .sum
  }

  def peek[T](x: T): T = {
    println(x);
    x
  }


}

class Day3Test extends AnyFunSuiteLike {

  test("day 3 part 1") {
    assert(Day3.part1(
      """987654321111111
        |811111111111119
        |234234234234278
        |818181911112111""".stripMargin.linesIterator)
      === 357)
  }

  test("day 3 part 2") {
    assert(Day3.part2(
      """987654321111111
        |811111111111119
        |234234234234278
        |818181911112111""".stripMargin.linesIterator)
      === 3121910778619L)
  }

}
