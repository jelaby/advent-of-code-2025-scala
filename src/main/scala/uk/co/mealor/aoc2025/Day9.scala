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

    val redTiles = lines.map(line => line.split(",").map(_.toInt) match {
        case Array(x, y) => (x, y)
      })
      .toList.to(SortedSet)

    redTiles.flatMap(a => redTiles.map(b => ((b(0) - a(0)).abs + 1L) * ((b(1) - a(1)).abs + 1L))).max
  }

  def part2(lines: Iterator[String]): Long = {

    trait Line {
      def crosses(other: Line): Boolean

      def length: Int
    }

    case class HLine(x: Int, y: Int, l: Int) extends Line {
      def crosses(other: Line): Boolean = {
        other match {
          case HLine(_, _, _) => false
          case VLine(ox, oy, ol) => x <= ox && x + l >= ox && y >= oy && y <= oy + ol
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

    trait Corner {
      def isCrossedBy(line: Line): Boolean
    }

    def toSimpleLine(a: (Int, Int), b: (Int, Int)): Line = {
      if a(0) == b(0) then
        VLine(a(0), a(1) min b(1), (b(1) - a(1)).abs)
      else if a(1) == b(1) then
        HLine(a(0) min b(0), a(1), (b(0) - a(0)).abs)
      else
        throw new IllegalArgumentException(s"$a -> $b is not horizontal or vertical")
    }

    def toLine(prev: (Int, Int), a: (Int, Int), b: (Int, Int), next: (Int, Int)): Line = {
      if a(0) == b(0) then {
        val x = a(0)

        val isDown = a(1) < b(1)
        val nextIsLeftwards = x > next(0)
        val prevIsRightwards = prev(0) < x
        val shortenEnd = if isDown == nextIsLeftwards then 0 else 1
        val shortenStart = if isDown == prevIsRightwards then 0 else 1
        val dx = if isDown then 1 else -1
        val dy = if isDown then shortenStart else -shortenStart

        toSimpleLine((x+dx, a(1) + (if isDown then shortenStart else -shortenStart)), (x+dx, b(1) + (if isDown then -shortenEnd else shortenEnd)))
      } else if a(1) == b(1) then {
        val y = a(1)

        val isRight = a(0) < b(0)
        val nextIsDownwards = y < next(1)
        val prevIsUpwards = prev(1) > y
        val shortenEnd = if isRight == nextIsDownwards then 0 else 1
        val shortenStart = if isRight == prevIsUpwards then 0 else 1
        val dx = if isRight then shortenStart else -shortenStart
        val dy = if isRight then -1 else 1

        toSimpleLine((a(0) + (if isRight then shortenStart else -shortenStart), y + dy), (b(0) + (if isRight then -shortenEnd else shortenEnd), y + dy))
      } else
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
            toSimpleLine(a, (a(0), b(1))),
            toSimpleLine((a(0), b(1)), b),
            toSimpleLine(b, (b(0), a(1))),
            toSimpleLine((b(0), a(1)), a),
          ),
          ((b(0) - a(0)).abs + 1L) * ((b(1) - a(1)).abs + 1L))
        ))
      .sortBy(-_(1))

    def redTile(i:Int): (Int,Int) = {
      redTiles(i % redTiles.size + (if i < 0 then redTiles.size else 0))
    }

    val edges = redTiles.indices
      .map(i => toLine(redTile(i-1),redTile(i),redTile(i+1),redTile(i+2)))
      .sortBy(-_.length)

    //(0 until redTiles.map(_(1)).max+2).foreach(y =>{
    //  (0 until redTiles.map(_(0)).max+2).foreach(x => {
    //    print(if redTiles contains (x,y) then
    //      '#'
    //    else if edges.exists(edge => HLine(x,y,0).crosses(edge) || VLine(x,y,0).crosses(edge)) then
    //      '*'
    //    else
    //      ' ')
    //  })
    //  println()
    //})

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

  test("day 9 part 2 eg 1") {
    assert(Day9.part2(
      """1,1
        |10,1
        |10,10
        |9,10
        |9,2
        |1,2""".stripMargin.linesIterator)
      === 20)
  }

}
