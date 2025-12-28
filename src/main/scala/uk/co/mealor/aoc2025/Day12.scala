package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike
import os.Path
import uk.co.mealor.aoc2025.Day12.{Shape, variants}

import java.util.concurrent.{Executors, ForkJoinPool}
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ForkJoinTaskSupport
import scala.util.matching.Regex

object Day12 {

  @main
  def runDay12() = {

    println(part1(scala.io.Source.fromResource("day12.txt").getLines()))
    println(part2(scala.io.Source.fromResource("day12.txt").getLines()))

  }

  def variants(shape: Shape): Set[Shape] = Set(
      shape,
      shape.rotate,
      shape.rotate.rotate,
      shape.rotate.rotate.rotate,
      shape.hflip,
      shape.hflip.rotate,
      shape.hflip.rotate.rotate,
      shape.hflip.rotate.rotate.rotate
    )

  case class Shape(shape: List[List[Boolean]]) {

    val area: Int = shape.flatten.count(i => i)

    def apply(x: Int, y: Int): Boolean = {
      if y < 0 || y >= shape.size || x < 0 || x >= shape.head.size then
        false
      else
        shape(y)(x)
    }

    def width: Int = {
      shape.head.size
    }

    def height: Int = {
      shape.size
    }

    def rotate: Shape = {
      Shape((0 until width)
        .map(y => (0 until height)
          .map(x => this.apply(y,height - 1 - x))
          .toList
        )
        .toList)
    }

    def hflip: Shape = {
      Shape(shape.map(_.reverse))
    }

    def vflip: Shape = {
      Shape(shape.reverse)
    }

    override def toString: String = shape.map(row => row.map(if _ then '#' else '.').mkString).mkString("\n")
  }

  case class Region(width: Int, height: Int, requiredShapes: List[Int]) {

    def area: Int = width * height

  }

  def round(n: Int, t: Int) = {
    (n / t) * t
  }

  def part1(lines: Iterator[String]): Long = {

    val (shapes, regions) = parse(lines)

    regions.zipWithIndex.count((region, i) => {

        if region.requiredShapes.zipWithIndex.map((n,i) => shapes(i).area * n).sum > region.area then {
          false
        } else if region.requiredShapes.zipWithIndex.map((n,i) => shapes(i).width * shapes(i).height * n).sum <= round(region.width, 3) * round(region.height, 3) then
          true
        else {
          throw new IllegalArgumentException("Non-trivial")
        }
    })
  }

  def part2(lines: Iterator[String]): Long = {
    -1
  }

  def SHAPE_HEADER: Regex = "^\\d+:$".r

  def REGION_PATTERN: Regex = "(\\d+)x(\\d+): (\\d+(?: \\d+)*)".r

  def parse(lines: Iterator[String]): (List[Shape], List[Region]) = {
    @tailrec
    def parseImpl(lines: List[String], shapes: List[Shape], regions: List[Region]): (List[Shape], List[Region]) = {

      if lines.isEmpty then
        (shapes.reverse, regions.reverse)
      else {

        if SHAPE_HEADER.matches(lines.head) then {
          val shape = Shape(lines.tail.takeWhile(_.nonEmpty).map(line => line.map(_ == '#').toList))
          val remainder = lines.dropWhile(_.nonEmpty).tail
          parseImpl(remainder, shape :: shapes, regions)
        } else
          lines.head match {
            case REGION_PATTERN(w, h, requirements) =>
              parseImpl(lines.tail, shapes, Region(w.toInt, h.toInt, requirements.split(' ').map(_.toInt).toList) :: regions)
            case _ => throw new IllegalArgumentException(s"Unrecognised line ${lines.head}")
          }
      }
    }

    parseImpl(lines.toList, List(), List())
  }

  def peek[T](x: T): T = {
    println(x);
    x
  }


}

class Day12Test extends AnyFunSuiteLike {

  test("variants 1") {
    println(Shape(List(List(true,true,false), List(false,false,false), List(false,false,false), List(true,false,false))))
    println("=====")
    println(variants(Shape(List(List(true,true,false), List(false,false,false), List(false,false,false), List(true,false,false)))).mkString("\n\n"))
    assert(variants(Shape(List(List(true,true,false), List(false,false,false), List(true,false,false)))).size === 8)
    assert(variants(Shape(List(List(true,false,false), List(false,false,false), List(false,false,false)))).size === 4)
  }

  test("hflip") {
    assert(Shape(List(List(true,false,false), List(false,false,false), List(false,false,false), List(true,true,false))).hflip
      === Shape(List(List(false,false,true), List(false,false,false), List(false,false,false), List(false,true,true))))
  }

  test("vflip") {
    assert(Shape(List(List(true,false,false), List(false,false,false), List(false,false,false), List(true,true,false))).vflip
    === Shape(List(List(true,true,false), List(false,false,false), List(false,false,false), List(true,false,false))))
  }

  test("rotate") {
    assert(Shape(List(List(true,false,false), List(false,false,false), List(false,false,false), List(true,true,false))).rotate
      === Shape(List(List(true,false,false,true), List(true,false,false,false), List(false,false,false,false))))
  }
  
  test("round") {
    assert(List(1,2,3,4,5,6,7,8,9).map(Day12.round(_, 3)) === List(0,0,3,3,3,6,6,6,9))
  }

  test("day 12 part 1") {
    assert(Day12.part1(
      """0:
        |###
        |##.
        |##.
        |
        |1:
        |###
        |##.
        |.##
        |
        |2:
        |.##
        |###
        |##.
        |
        |3:
        |##.
        |###
        |##.
        |
        |4:
        |###
        |#..
        |###
        |
        |5:
        |###
        |.#.
        |###
        |
        |4x4: 0 0 0 0 2 0
        |12x5: 1 0 1 0 2 2
        |12x5: 1 0 1 0 3 2""".stripMargin.linesIterator)
      === 2)
  }

  test("day 12 part 2") {
    assert(Day12.part2(
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
      === 99)
  }
}
