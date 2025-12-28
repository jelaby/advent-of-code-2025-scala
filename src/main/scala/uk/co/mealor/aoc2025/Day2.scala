package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike

object Day2 {

  @main
  def runDay2() = {

    println(part1(scala.io.Source.fromResource("day2-part1.txt").getLines().toList.head))
    println(part2(scala.io.Source.fromResource("day2-part1.txt").getLines().toList.head))

  }

  def part1(line: String): Long = {

    line.split(',').iterator
      .map(i => i.split('-').map(s => s.toLong))
      .flatMap(r => r(0) until r(1) + 1)
      .filter(i => {
        val s = i.toString;
        s.substring(0,s.length/2) == s.substring(s.length/2,s.length)
      })
      .sum();


  }

  def part2(line: String): Long = {

    line.split(',').iterator
      .map(i => i.split('-').map(s => s.toLong))
      .flatMap(r => r(0) until r(1) + 1)
      .filter(i => {
        val s = i.toString
        (1 until s.length)
          .filter(l => s.length % l == 0)
          .exists(l => (0 until s.length by l).map(o => s.substring(o,o+l)).toSet.size == 1)
      })
      .sum();


  }

  def peek[T](x: T): T = {
    println(x);
    x
  }


}

class Day2Test extends AnyFunSuiteLike {

  test("day 2 part 1") {
    assert(Day2.part1("11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")
      === 1227775554)
  }

  test("day 2 part 2") {
    assert(Day2.part2("11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")
      === 4174379265L)
  }

}
