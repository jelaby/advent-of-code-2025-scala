package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike
import scalaz.Memo

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object Day9 {

  @main
  def runDay9() = {

    println(part1(scala.io.Source.fromResource("day9.txt").getLines()))
    println(part2(scala.io.Source.fromResource("day9.txt").getLines()))

  }


  def part1(lines: Iterator[String]): Long = {

    val redTiles = lines.map(line => line.split(",").map(_.toInt) match {case Array(x,y) => (x,y)})
      .toList.to(SortedSet)

    redTiles.flatMap(a => redTiles.map(b => ((b(0)-a(0)).abs+1L) * ((b(1)-a(1)).abs + 1L))).max
  }

  def part2(lines: Iterator[String]): Long = {

    trait Line {
      def crosses(other: Line): Boolean
      def length: Int
    }

    case class HLine(x: Int, y: Int, l: Int) extends Line {
      def crosses(other: Line): Boolean = {
        other match {
            case HLine(_,_,_) => false
            case VLine(ox,oy,ol) => x < ox && x + l > ox && y >= oy && y <= oy + ol
        }
      }
      def length: Int = l
    }

    case class VLine(x: Int, y: Int, l: Int) extends Line {
      def crosses(other: Line): Boolean = {
        other match {
          case VLine(_,_,_) => false
          case o: HLine => o.crosses(this)
        }
      }
      def length: Int = l
    }

    def toLine(a: (Int,Int), b: (Int,Int)): Line = {
      if a(0) == b(0) then
        VLine(a(0), a(1) min b(1), (b(1) - a(1)).abs)
      else if a(1) == b(1) then
        HLine(a(0) min b(0), a(1), (b(0) - a(0)).abs)
      else
        throw new IllegalArgumentException(s"$a -> $b is not horizontal or vertical")
    }

    val redTiles = lines.map(line => line.split(",").map(_.toInt) match {
        case Array(x, y) => (x, y)
      })
      .toList

    val rectangles = redTiles.flatMap(a => redTiles
        .filter(a != _)
        .map(b => (
          List(
            toLine(a, (a(0),b(1))),
            toLine((a(0),b(1)), b),
            toLine(b, (b(0),a(1))),
            toLine((b(0),a(1)), a),
          ),
          ((b(0) - a(0)).abs + 1L) * ((b(1) - a(1)).abs + 1L))
        ))
      .sortBy(-_(1))

    val edges = redTiles.sliding(2).map(p => toLine(p(0), p(1))).toList
      .sortBy(-_.length)

    rectangles.iterator
      .filter((sides, _) => !sides.exists(side => edges.exists(edge => edge.crosses(side))))
      .next()(1)

  }

  def peek[T](x: T): T = {
    println(x);
    x
  }


}

class Day9Test extends AnyFunSuiteLike {

  test("day 9 part 1") {
    assert(Day9.part1(
      """7,1
        |11,1
        |11,7
        |9,7
        |9,5
        |2,5
        |2,3
        |7,3""".stripMargin.linesIterator)
      === 50)
  }


  test("day 9 part 2") {
    assert(Day9.part2(
      """7,1
        |11,1
        |11,7
        |9,7
        |9,5
        |2,5
        |2,3
        |7,3""".stripMargin.linesIterator)
      === 24)
  }

}
