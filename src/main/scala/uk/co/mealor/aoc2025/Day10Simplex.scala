package uk.co.mealor.aoc2025

import org.scalatest.funsuite.AnyFunSuiteLike
import os.{Path, RelPath}

import java.util.concurrent.ForkJoinPool
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ForkJoinTaskSupport

object Day10Simplex {

  @main
  def runDay10Simplex() = {

    //println(part1(scala.io.Source.fromResource("day10.txt").getLines()))
    println(part2(scala.io.Source.fromResource("day10.txt").getLines(), "main"))

  }

  def parse(line: String): (BitSet, List[BitSet], List[Int]) = {

    val parts = line.split(" ").toList

    val target = parts.head.substring(1, parts.head.length - 1)
      .iterator
      .zipWithIndex
      .flatMap((c, i) => c match {
        case '.' => Iterator()
        case '#' => Iterator(i)
      })
      .to(BitSet)

    val (buttonStrings, voltageStrings) = parts.tail.partition(_(0) == '(')

    val buttons = buttonStrings.iterator
      .map(s => s.substring(1, s.length - 1))
      .map(_.split(",").map(_.toInt).to(BitSet))
      .toList

    val voltages = voltageStrings.iterator
      .map(s => s.substring(1, s.length - 1))
      .map(_.split(",").map(_.toInt).toList)
      .next()

    (target, buttons, voltages)
  }

  def solve(target: BitSet, buttons: List[BitSet]): Long = {
    solve(target, buttons.toSet, BitSet.empty, Integer.MAX_VALUE)
  }

  def solve(target: BitSet, buttons: Set[BitSet], current: BitSet, bestSoFar: Long): Long = {

    if current == target then
      0
    else if buttons.isEmpty || bestSoFar == 0 then
      Integer.MAX_VALUE
    else {
      1 + buttons.iterator
        .foldLeft(Integer.MAX_VALUE.toLong)((best, button) => best min solve(target, buttons - button, current ^ button, (bestSoFar min best) - 1))
    }
  }

  object Tableau {

    def toMinimiseSlack(c: List[Int], b: List[Double], a: List[List[Double]]) = {
      new Tableau((0 until 1 + b.size).map(row =>
        (0 until 1 + c.size + b.size + 1).map[Double]({
          // z column
          case col if (col == 0) => if row == 0 then 1 else 0
          case col if (col > 0 && col <= c.size) => if row == 0 then
            0
          else
            a(row - 1)(col - 1)
          case col if (col > c.size && col <= c.size + b.size) => if col - c.size == row then 1 else if row == 0 then 1000 else 0
          case col => if row == 0 then 0 else b(row - 1)
        }).toList).toList)
    }

    def withSlack(c: List[Double], b: List[Double], a: List[List[Double]], slackCost: Double = 0) = {
      new Tableau((0 until 1 + b.size).map(row =>
        (0 until 1 + c.size + b.size + 1).map[Double]({
          // z column
          case col if (col == 0) => if row == 0 then 1 else 0
          case col if (col > 0 && col <= c.size) => if row == 0 then
            - c(col - 1)
          else
            a(row - 1)(col - 1)
          case col if (col > c.size && col <= c.size + b.size) => if col - c.size == row then 1 else if row == 0 then slackCost else 0
          case col => if row == 0 then 0 else b(row - 1)
        }).toList).toList)
    }


  }

  class Tableau(tableau: List[List[Double]]) {

    def at(col: Int, row: Int): Double = tableau(row)(col)


    def columnAt(col: Int): List[Double] = rowIndices.map(row => at(col,row)).toList

    def rowAt(row: Int): List[Double] = tableau(row)

    override def toString: String = {
      tableau.map(_.map(n => if n == 0 then "         " else f"$n%9.4f").mkString("[", "\t", "]")).mkString("\n")
    }

    def toArray: List[List[Double]] = {
      tableau
    }

    def colIndices: Range = tableau.head.indices

    def columns: Int = tableau.head.size

    def lastColumn: Int = columns - 1

    def rowIndices: Range = tableau.indices

    def rows: Int = tableau.size
    
    def basicColumns: List[Int] = colIndices
      .filter(_ > 0)
      .filter(_ < lastColumn)
      .filter(col => rowIndices.count(row => at(col,row).==(1)) == 1 && rowIndices.count(row => !(0 until 1).inclusive.contains(at(col,row))) == 0)
      .toList

    def maximise: Tableau = {
      operate(_ < 0)
    }

    def minimise: Tableau = {
      operate(_ > 0)
    }

    def operate(test: Double => Boolean): Tableau = {
      if !tableau.head.tail.take(tableau.head.length - 2).exists(test) then
        println(s"Finished")
        println(toString)
        this
      else
        // Select a column with a negative first row
        colIndices
          .filter(_ > 0)
          .filter(_ < columns - 1)
          .find(c => test(at(c, 0)))
          .map(pivotCol => {

            val pivotRow = rowIndices
              .filter(_ > 0)
              .minBy(r => {
                if at(pivotCol, r) == 0 then Int.MaxValue else {
                  val v = at(lastColumn, r) / at(pivotCol, r)
                  assert(v * at(pivotCol, r) == at(lastColumn, r))
                  if v < 0 then Int.MaxValue else v
                }
              })

            println(s"Pivot ($pivotCol, $pivotRow)")
            println(toString)

            // TODO:
            // normalise row to make at(pivotCol,pivotRow) 1
            // subtract row from all other rows to make column "basic"

            val pivotRowValues = tableau(pivotRow).map(_ / at(pivotCol, pivotRow))

            new Tableau(tableau.zipWithIndex
              .map((row, ir) => if ir == pivotRow then
                pivotRowValues
              else
                row.zipWithIndex.map((cell, col) => cell - pivotRowValues(col) * row(pivotCol)))).operate(test)
          })
          .getOrElse(this)
    }

    def withoutSlack: Tableau = {
      new Tableau(tableau
        .map(row => row.zipWithIndex
          .filter((c, i) => i < lastColumn - (rows - 1) || i == lastColumn)
          .map((c, i) => c)))
    }

  }

  def flip[T](a: List[List[T]]): List[List[T]] = {
    a.head.indices.map(col => a.indices.map(row => a(row)(col)).toList).toList
  }


  def solve2(name: String, target: List[Int], buttons: List[BitSet]): Long = {



    // Maximise to eliminate slack

    -1


  }

  /*
  3,5,4,7
  3,4,4,6
  3,3,4,5
  3,3,3,4
  3,2,3,3
  3,2,2,2
  2,2,1,2
  1,1,1,1
  0,0,1,1
  0,0,0,0
   */
  /*
  7,5,12,7,2
  6,5,11,6,1
  5,5,10,5,0
  4,4,9,5,0
  4,4,8,4,0
  3,3,7,4,0
  3,3,6,3,0
  2,2,5,3
  2,2,4,2
  1,1,3,2
  1,1,2,1
  0,0,1,1
  0,0,0,0
   */

  def part1(lines: Iterator[String]): Long = {

    lines.map(parse)
      .zipWithIndex.map((l, i) => peek((i, l))).map((i, l) => l)
      .map((target, buttons, _) => solve(target, buttons)).sum

  }

  def part2(lines: Iterator[String], name: String): Long = {

    lines.map(parse)
      .zipWithIndex.map((items, i) => {
        val (_, buttons, target) = items;
        (i, buttons, target)
      })
      .map((i, buttons, target) => solve2(s"$name $i", target, buttons))
      .sum
  }

  def peek[T](x: T): T = {
    println(x);
    x
  }

}

class Day10SimpleTest extends AnyFunSuiteLike {

  test("day 10 part 1") {
    assert(Day10Simplex.part1(
      """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
        |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
        |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin.linesIterator)
      === 7)
  }

  test("day 10 part 2") {
    assert(Day10Simplex.part2(
      """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
        |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
        |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin.linesIterator,
      "day 10 part 2")
      === 33)
  }

  test("day 10 part 2 eg 1") {
    assert(Day10Simplex.part2(
      """[...##] (0,1) (1,2) (2,3) (3,4) {1,2,2,2,1}""".stripMargin.linesIterator,
      "day 10 part 2 eg 1")
      === 4)
  }

  test("day 10 part 2 eg 2") {
    assert(Day10Simplex.part2(
      """[...##] (0) (0,1) (0,1,2) (0,1,2,3) {1,0,0,0}""".stripMargin.linesIterator,
      "day 10 part 2 eg 2")
      === 1)
  }

  test("day 10 part 2 eg 3") {
    assert(Day10Simplex.part2(
      """[...##] (0) (0,1) (0,1,2) (0,1,2,3) {2,2,1,1}""".stripMargin.linesIterator,
      "day 10 part 2 eg 3")
      === 2)
  }

  test("tableau constructor") {
    assert(Day10Simplex.Tableau.withSlack(
      List(1, 1, 1, 1, 1, 1),
      List(3, 5, 4, 7), Day10Simplex.flip(
        List(
          List(0, 0, 0, 1),
          List(0, 1, 0, 1),
          List(0, 0, 1, 0),
          List(0, 0, 1, 1),
          List(1, 0, 0, 1),
          List(1, 1, 0, 0)))).toString ===
      s"""[1\t-1\t-1\t-1\t-1\t-1\t-1\t\t\t\t\t]
         |[\t\t\t\t\t1\t1\t1\t\t\t\t3]
         |[\t\t1\t\t\t\t1\t\t1\t\t\t5]
         |[\t\t\t1\t1\t\t\t\t\t1\t\t4]
         |[\t1\t1\t\t1\t1\t\t\t\t\t1\t7]""".stripMargin.replaceAll("\r\n", "\n")
    )
  }

  test("tableau withoutSlack") {
    assert(Day10Simplex.Tableau.withSlack(
      List(1, 1, 1, 1, 1, 1),
      List(3, 5, 4, 7), Day10Simplex.flip(
        List(
          List(0, 0, 0, 1),
          List(0, 1, 0, 1),
          List(0, 0, 1, 0),
          List(0, 0, 1, 1),
          List(1, 0, 0, 1),
          List(1, 1, 0, 0)))).withoutSlack.toString ===
      s"""[1\t-1\t-1\t-1\t-1\t-1\t-1\t]
         |[\t\t\t\t\t1\t1\t3]
         |[\t\t1\t\t\t\t1\t5]
         |[\t\t\t1\t1\t\t\t4]
         |[\t1\t1\t\t1\t1\t\t7]""".stripMargin.replaceAll("\r\n", "\n")
    )
  }

  test("tableau maximise to remove slack") {
    val tableau = Day10Simplex.Tableau.withSlack(
      List(1, 1, 1, 1, 1, 1),
      List(3, 5, 4, 7), Day10Simplex.flip(
        List(
          List(0, 0, 0, 1),
          List(0, 1, 0, 1),
          List(0, 0, 1, 0),
          List(0, 0, 1, 1),
          List(1, 0, 0, 1),
          List(1, 1, 0, 0))))
    assert(tableau.maximise.at(11,0) ===
      16
    )
  }

  test("tableau to minimize slack") {
    val tableau = Day10Simplex.Tableau.withSlack(
      List(1, 1, 1, 1, 1, 1),
      List(3, 5, 4, 7), Day10Simplex.flip(
        List(
          List(0, 0, 0, 1),
          List(0, 1, 0, 1),
          List(0, 0, 1, 0),
          List(0, 0, 1, 1),
          List(1, 0, 0, 1),
          List(1, 1, 0, 0))), -1.0 / 1024)
    assert(tableau.maximise.at(11,0) ===
      0
    )
  }

  test("tableau minimize with slack") {
    val tableau = Day10Simplex.Tableau.withSlack(
      List(1, 1, 1, 1, 1, 1),
      List(3, 5, 4, 7), Day10Simplex.flip(
        List(
          List(0, 0, 0, 1),
          List(0, 1, 0, 1),
          List(0, 0, 1, 0),
          List(0, 0, 1, 1),
          List(1, 0, 0, 1),
          List(1, 1, 0, 0))))
    assert(tableau.minimise.at(11,0) ===
      0
    )
  }

  test("tableau minimise") {
    val buttonDefinitions: List[List[Double]] = List(
      List(0, 0, 0, 1),
      List(0, 1, 0, 1),
      List(0, 0, 1, 0),
      List(0, 0, 1, 1),
      List(1, 0, 0, 1),
      List(1, 1, 0, 0))
    val targetValues = List[Double](3, 5, 4, 7)
    val minimum = Day10Simplex.Tableau.withSlack(
      List(1, 1, 1, 1, 1, 1),
      targetValues, Day10Simplex.flip(
        buttonDefinitions)).maximise.withoutSlack.minimise

    val resultColumn = minimum.columnAt(minimum.lastColumn)
    
    val coefficients = minimum.basicColumns.map(i => (minimum.columnAt(i).tail.zip(resultColumn.tail).map(_ * _).sum, i))

    val calculatedValues = coefficients.map((v,i) => buttonDefinitions(i).map(_ * v))
      .foldLeft(List[Double](0,0,0,0))((acc,v) => acc.zip(v).map(_ + _))
    
    assert(calculatedValues === targetValues)
    
  }
}
