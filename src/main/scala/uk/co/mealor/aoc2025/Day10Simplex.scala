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

    // @formatter:off
    /**
     * Minimise Z
     * given
     * Z = ∑ci·xi => maximise Z' = ∑ci·x'i - ∑ci·ximax
     * bj = ∑ aij·xi => ∑ aij·(ximax - x'i)
     * xi ≥ 0 , xi ≤ ximax => xi = ximax - x'i | x'i ≥ 0 , x'i ≤ ximax
     *
     * <pre><!-- @noformat
     * 1   -c1 -c2 ... -cn  0   0  ... 0   -∑ximax
     *     a11 a21 ... an1 :              b1
     *     a21 a22 ... an2                b2
     *      :   :       :                 :
     *     an1 an2 ... ann                bn
     *     1               1              x1max
     *         1               1          x2max
     *             ˙·.            ˙·.     :
     *                 1               1  xnmax
     * </pre>
     *
     * Note this was actually only tested with all cn=1
     */
    // @formatter:on
    def toMinimise(c: List[Double], b: List[Double], a: List[List[Double]], xmax: List[Double]) = {
      a.foreach(r => assert(c.size == r.size, s"${c.size} == ${r.size}"))
      assert(b.size == a.size)
      assert(xmax.size == c.size)
      new Tableau((0 until 1 + b.size + xmax.size).map(row =>
        (0 until 1 + c.size + xmax.size + 1).map[Double]({
          // z column
          case col if (col == 0) => if row == 0 then 1 else 0
          case col if (col > 0 && col <= c.size) =>
            if row == 0 then {
              -c(col - 1)
            } else if row <= a.size then {
              a(row - 1)(col - 1)
            } else if row - a.size == col then {
              1
            } else {
              0
            }
          case col if (col > c.size && col <= c.size + xmax.size) =>
            if col - c.size == row - b.size then 1
            else 0
          case col =>
            if row == 0 then
              -xmax.sum
            else if row <= b.size then
              b(row - 1)
            else
              xmax(row - b.size - 1)
        }).toList).toList)
    }

    def toMaximise(c: List[Double], b: List[Double], a: List[List[Double]]) = {
      new Tableau((0 until 1 + b.size).map(row =>
        (0 until 1 + c.size + 1).map[Double]({
          // z column
          case col if (col == 0) => if row == 0 then 1 else 0
          case col if (col > 0 && col <= c.size) => if row == 0 then
            -c(col - 1)
          else
            a(row - 1)(col - 1)
          case col => if row == 0 then 0 else b(row - 1)
        }).toList).toList)
    }

  }

  class Tableau(tableau: List[List[Double]]) {

    def at(col: Int, row: Int): Double = tableau(row)(col)


    def columnAt(col: Int): List[Double] = rowIndices.map(row => at(col, row)).toList

    def rowAt(row: Int): List[Double] = tableau(row)

    override def toString: String = {
      //tableau.map(_.map(n => if n == 0 then "         " else f"$n%9.4f").mkString("[", "\t", "]")).mkString("\n")
      tableau.map(_.map(n => if n == 0 then "" else f"$n%1.0f").mkString("[", "\t", "]")).mkString("\n")
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
      .filter(col => rowIndices.count(row => at(col, row).==(1)) == 1 && rowIndices.count(row => !(0 until 1).inclusive.contains(at(col, row))) == 0)
      .toList

    def maximise: Tableau = {
      operate(_ < 0)
    }

    def minimise: Tableau = {
      operate(_ > 0)
    }

    def operate(test: Double => Boolean): Tableau = {
      println(toString)
      if !tableau.head.tail.take(tableau.head.length - 2).exists(test) then
        println(s"Finished")
        this
      else
        // Select a column with a negative first row
        colIndices
          .filter(_ > 0)
          .filter(_ < columns - 1)
          .find(c => test(at(c, 0)))
          .map(pivotCol => {

            val (_, pivotRow) = rowIndices
              .filter(_ > 0)
              .filter(at(pivotCol, _) != 0)
              .map(r => {
                val v = at(lastColumn, r) / at(pivotCol, r)
                assert(v * at(pivotCol, r) == at(lastColumn, r))
                (v, r)
              })
              .filter((v, _) => v > 0)
              .minBy((v, _) => v)

            println(s"Pivot ($pivotCol, $pivotRow)")

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

  test("tableau to maximise") {
    assert(Day10Simplex.Tableau.toMaximise(
      List(1, 1, 1, 1, 1, 1),
      List(3, 5, 4, 7), Day10Simplex.flip(
        List(
          List(0, 0, 0, 1),
          List(0, 1, 0, 1),
          List(0, 0, 1, 0),
          List(0, 0, 1, 1),
          List(1, 0, 0, 1),
          List(1, 1, 0, 0)))).toString ===
      s"""[1\t-1\t-1\t-1\t-1\t-1\t-1\t]
         |[\t\t\t\t\t1\t1\t3]
         |[\t\t1\t\t\t\t1\t5]
         |[\t\t\t1\t1\t\t\t4]
         |[\t1\t1\t\t1\t1\t\t7]""".stripMargin.replaceAll("\r\n", "\n")
    )
  }

  test("tableau toMinimise") {
    assert(Day10Simplex.Tableau.toMinimise(
      List(1, 1, 1, 1, 1, 1),
      List(3, 5, 4, 7),
      Day10Simplex.flip(
        List(
          List(0, 0, 0, 1),
          List(0, 1, 0, 1),
          List(0, 0, 1, 0),
          List(0, 0, 1, 1),
          List(1, 0, 0, 1),
          List(1, 1, 0, 0))),
      List(7, 5, 4, 4, 3, 3)).toString ===
      s"""[1\t-1\t-1\t-1\t-1\t-1\t-1\t\t\t\t\t\t\t-26]
         |[\t\t\t\t\t1\t1\t\t\t\t\t\t\t3]
         |[\t\t1\t\t\t\t1\t\t\t\t\t\t\t5]
         |[\t\t\t1\t1\t\t\t\t\t\t\t\t\t4]
         |[\t1\t1\t\t1\t1\t\t\t\t\t\t\t\t7]
         |[\t1\t\t\t\t\t\t1\t\t\t\t\t\t7]
         |[\t\t1\t\t\t\t\t\t1\t\t\t\t\t5]
         |[\t\t\t1\t\t\t\t\t\t1\t\t\t\t4]
         |[\t\t\t\t1\t\t\t\t\t\t1\t\t\t4]
         |[\t\t\t\t\t1\t\t\t\t\t\t1\t\t3]
         |[\t\t\t\t\t\t1\t\t\t\t\t\t1\t3]"""
        .stripMargin.replaceAll("\r\n", "\n")
    )
  }

  test("tableau maximise") {
    assert(Day10Simplex.Tableau.toMaximise(
        List(1, 1, 1, 1, 1, 1),
        List(3, 5, 4, 7), Day10Simplex.flip(
          List(
            List(0, 0, 0, 1),
            List(0, 1, 0, 1),
            List(0, 0, 1, 0),
            List(0, 0, 1, 1),
            List(1, 0, 0, 1),
            List(1, 1, 0, 0))))
      .at(7, 0) === 16)
  }

  test("tableau minimise 1") {
    val minimum = Day10Simplex.Tableau.toMinimise(
        List(1, 1, 1, 1, 1, 1),
        List(6 - 3, 8 - 5, 8 - 4, 19 - 7),
        Day10Simplex.flip(
          List(
            List(0, 0, 0, 1),
            List(0, 1, 0, 1),
            List(0, 0, 1, 0),
            List(0, 0, 1, 1),
            List(1, 0, 0, 1),
            List(1, 1, 0, 0))),
        List(7, 5, 4, 4, 3, 3))
      .maximise
    assert(minimum.at(minimum.lastColumn, 0) == -10
    )
  }

  test("tableau minimise 2") {
    val minimum = Day10Simplex.Tableau.toMinimise(
        List(1, 1, 1, 1, 1),
        List(2 + 2 + 5 - 7, 7 + 2 - 5, 2 + 7 + 5 + 2 - 12, 2 + 7 + 0 + 0 + 2 - 7, 2 + 2 + 2 - 2),
        Day10Simplex.flip(
          List(
            List(1, 0, 1, 1, 1),
            List(0, 0, 1, 1, 0),
            List(1, 0, 0, 0, 1),
            List(1, 1, 1, 0, 0),
            List(0, 1, 1, 1, 1))),
        List(2, 7, 2, 5, 2))
      .maximise
    assert(minimum.at(minimum.lastColumn, 0) == -12
    )
  }

  test("tableau minimise 3") {
    val minimum = Day10Simplex.Tableau.toMinimise(
        List(1, 1, 1, 1),
        List(5 + 5 + 5 - 10, 5 + 5 + 11 - 11, 5 + 5 + 11 - 11, 5 + 5 - 5, 5 + 5 + 5 - 10, 5 - 5),
        Day10Simplex.flip(
          List(
            List(1, 1, 1, 1, 1, 0),
            List(1, 0, 0, 1, 1, 0),
            List(1, 1, 1, 0, 1, 1),
            List(0, 1, 1, 0, 0, 0))),
        List(5, 5, 5, 11))
      .maximise
    assert(minimum.at(minimum.lastColumn, 0) == -11
    )
  }
}
