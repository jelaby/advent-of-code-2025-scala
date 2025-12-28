package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike
import scalaz.Memo

import scala.annotation.tailrec

object Day8 {

  @main
  def runDay8() = {

    println(part1(scala.io.Source.fromResource("day8.txt").getLines(), 1000))
    println(part2(scala.io.Source.fromResource("day8.txt").getLines()))

  }


  def part1(lines: Iterator[String], maxConnections: Int): Long = {

    val coords = lines.map(line => line.split(",").map(_.toLong)).toList;

    val distances = coords.zipWithIndex.flatMap((p1, i) => coords.zipWithIndex
        .filter((p2, j) => i > j)
        .map((p2, j) => ((i, j), p1.zip(p2).map((a, b) => (b - a) * (b - a)).sum)))
      .sortBy(_(1))
      .slice(0,maxConnections)

    def connect(connections: List[(Int, Int)]): List[Set[Int]] = {
      @tailrec
      def connectOne(connections: List[(Int, Int)], boxGroups: Map[Int, Int], groupBoxes: Map[Int, Set[Int]], nextGroup: Int): List[Set[Int]] = {
        if connections.isEmpty then
          groupBoxes.values.toList.sortBy(-_.size)
        else {
          val connection = connections.head
          val group = boxGroups.get(connection(0))
          val otherGroup = boxGroups.get(connection(1))
          val (newGroup, nextNextGroup) = group.orElse(otherGroup) match {
            case Some(g) => (g, nextGroup)
            case None => (nextGroup, nextGroup + 1)
          }

          val groupsToUpdate = group.iterator.concat(otherGroup.iterator).toSet
          val newBoxGroups = boxGroups.map((b, g) => if groupsToUpdate contains g then (b, newGroup) else (b, g))
            + (connection(0) -> newGroup) + (connection(1) -> newGroup)
          val combinedGroup = groupsToUpdate.map(groupBoxes(_)).fold(Set())(_ ++ _) + connection(0) + connection(1)
          val newGroupBoxes = (groupBoxes -- groupsToUpdate) + (newGroup -> combinedGroup)

          connectOne(connections.tail, newBoxGroups, newGroupBoxes, nextNextGroup)
        }
      }

      connectOne(connections, Map(), Map(), 1)
    }

    connect(distances.map(_(0))).slice(0, 3).map(_.size).map(_.toLong).product
  }

  def part2(lines: Iterator[String]): Long = {

    val coords = lines.map(line => line.split(",").map(_.toLong)).toList;

    val distances = coords.zipWithIndex.flatMap((p1, i) => coords.zipWithIndex
        .filter((p2, j) => i > j)
        .map((p2, j) => ((i, j), p1.zip(p2).map((a, b) => (b - a) * (b - a)).sum)))
      .sortBy(_(1))

    def connect(connections: List[(Int, Int)]): (Int,Int) = {
      @tailrec
      def connectOne(connections: List[(Int, Int)], boxGroups: Map[Int, Int], groupBoxes: Map[Int, Set[Int]], nextGroup: Int, result: Option[(Int,Int)]): (Int,Int) = {
        if connections.size % 100 == 0 then println(connections.size)

        if connections.isEmpty then
          result.get
        else {
          val connection = connections.head
          val group = boxGroups.get(connection(0))
          val otherGroup = boxGroups.get(connection(1))
          val (newGroup, nextNextGroup) = group.orElse(otherGroup) match {
            case Some(g) => (g, nextGroup)
            case None => (nextGroup, nextGroup + 1)
          }

          val nextResult = if group != otherGroup then Some(connection) else result

          val groupsToUpdate = group.iterator.concat(otherGroup.iterator).toSet
          val newBoxGroups = boxGroups.map((b, g) => if groupsToUpdate contains g then (b, newGroup) else (b, g))
            + (connection(0) -> newGroup) + (connection(1) -> newGroup)
          val combinedGroup = groupsToUpdate.map(groupBoxes(_)).fold(Set())(_ ++ _) + connection(0) + connection(1)
          val newGroupBoxes = (groupBoxes -- groupsToUpdate) + (newGroup -> combinedGroup)

          connectOne(connections.tail, newBoxGroups, newGroupBoxes, nextNextGroup, nextResult)
        }
      }

      connectOne(connections, Map(), Map(), 1, None)
    }

    val last = connect(distances.map(_(0)))
    coords(last(0))(0) * coords(last(1))(0)
  }

  def peek[T](x: T): T = {
    println(x);
    x
  }


}

class Day8Test extends AnyFunSuiteLike {

  test("day 8 part 1") {
    assert(Day8.part1(
      """162,817,812
        |57,618,57
        |906,360,560
        |592,479,940
        |352,342,300
        |466,668,158
        |542,29,236
        |431,825,988
        |739,650,466
        |52,470,668
        |216,146,977
        |819,987,18
        |117,168,530
        |805,96,715
        |346,949,466
        |970,615,88
        |941,993,340
        |862,61,35
        |984,92,344
        |425,690,689""".stripMargin.linesIterator, 10)
      === 40)
  }


  test("day 8 part 2") {
    assert(Day8.part2(
      """162,817,812
        |57,618,57
        |906,360,560
        |592,479,940
        |352,342,300
        |466,668,158
        |542,29,236
        |431,825,988
        |739,650,466
        |52,470,668
        |216,146,977
        |819,987,18
        |117,168,530
        |805,96,715
        |346,949,466
        |970,615,88
        |941,993,340
        |862,61,35
        |984,92,344
        |425,690,689""".stripMargin.linesIterator)
      === 25272)
  }

}
