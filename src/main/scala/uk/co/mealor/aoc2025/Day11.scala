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

    val map = parse(lines)

    @tailrec
    def solve(workList: List[String], cache: Map[String, Long], map: Map[String, Set[String]]):Long = {
      println(s"${workList}\t${cache}")

      if workList.isEmpty then
        cache("you")
      else /*cache.get(workList.head) match {
        case Some(_) => solve(workList.tail, cache, map)
        case None =>*/ {

          val outputs = map(workList.head)
          val notCalculated = outputs.filter(!cache.contains(_))

          if notCalculated.isEmpty then {
            val result = outputs.iterator.map(cache(_)).sum
            solve(workList.tail, cache + (workList.head -> result), map)
          } else {
            solve(notCalculated.toList ::: workList, cache, map)
          }
        }
      /*}*/

    }

    solve(List("you"), Map("out" -> 1), map)

  }

  def part2(lines: Iterator[String]): Long = {
0
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
      === 33)
  }
}
