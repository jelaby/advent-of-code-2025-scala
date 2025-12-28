package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike
import os.Path

import java.util.concurrent.{Executors, ForkJoinPool}
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ForkJoinTaskSupport

object Day11 {

  @main
  def runDay11() = {

    println(part1(scala.io.Source.fromResource("day11.txt").getLines()))
    println(part2(scala.io.Source.fromResource("day11.txt").getLines()))

  }


  def parse(lines: Iterator[String]): Map[String, Set[String]] = {
    lines.map(line => {
      line.split(": ") match {
        case Array(device, outputsString) => {
          (device, outputsString.split(" ").toSet)
        }
      }
    }).toMap
  }

  def part1(lines: Iterator[String]): Long = {
    solve(parse(lines), "you", "out")
  }

  def solve(map: Map[String, Set[String]], start: String, end: String): Long = {
    solve(List(start), Map(end -> 1), map, start)
  }

  @tailrec
  def solve(workList: List[String], cache: Map[String, Long], map: Map[String, Set[String]], start: String): Long = {

    if workList.isEmpty then
      cache(start)
    else {

      val outputs = map.getOrElse(workList.head, List())
      val notCalculated = outputs.filter(!cache.contains(_))

      if notCalculated.isEmpty then {
        val result = outputs.iterator.map(cache(_)).sum
        solve(workList.tail, cache + (workList.head -> result), map, start)
      } else {
        solve(notCalculated.toList ::: workList, cache, map, start)
      }
    }

  }

  def part2(lines: Iterator[String]): Long = {

    val map = parse(lines)

    (solve(map, "svr", "dac") * solve(map, "dac", "fft") * solve(map, "fft", "out"))
    + (solve(map, "svr", "fft") * solve(map, "fft", "dac") * solve(map, "dac", "out"))


  }

  def peek[T](x: T): T = {
    println(x);
    x
  }


}

class Day11Test extends AnyFunSuiteLike {

  test("day 11 part 1") {
    assert(Day11.part1(
      """aaa: you hhh
        |you: bbb ccc
        |bbb: ddd eee
        |ccc: ddd eee fff
        |ddd: ggg
        |eee: out
        |fff: out
        |ggg: out
        |hhh: ccc fff iii
        |iii: out""".stripMargin.linesIterator)
      === 5)
  }

  test("day 11 part 2") {
    assert(Day11.part2(
      """svr: aaa bbb
        |aaa: fft
        |fft: ccc
        |bbb: tty
        |tty: ccc
        |ccc: ddd eee
        |ddd: hub
        |hub: fff
        |eee: dac
        |dac: fff
        |fff: ggg hhh
        |ggg: out
        |hhh: out""".stripMargin.linesIterator)
      === 2)
  }
}
