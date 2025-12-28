package uk.co.mealor.aoc2025 {

  import org.scalatest.funsuite.AnyFunSuiteLike


  object Day1 {

    @main
    def runDay1(): Unit = {

      println(part1(scala.io.Source.fromResource("day1-part1.txt").getLines().toList, 50))
      println(part2(scala.io.Source.fromResource("day1-part1.txt").getLines().toList, 50))

    }

    def mod(a: Int, m: Int): Int = {
      (a % m) + (if (a % m) < 0 then m else 0)
    }

    def part1(lines: List[String], start: Int) : Int = {

      lines
        .collect(l => l.head match
          case 'L' => -Integer.parseInt(l.tail)
          case 'R' => Integer.parseInt(l.tail))
        .scanLeft(start)((r,v) => mod(r + v, 100))
        .count(i => i == 0)
    }

    def part2(lines: List[String], start : Int) : Int = {

      lines
        .collect(l => l.head match
          case 'L' => -Integer.parseInt(l.tail)
          case 'R' => Integer.parseInt(l.tail))
        .scanLeft((start, 0))((x,v) => {
          def next = mod(x(0) + v, 100)

         def passes = if v > 0 then
            (x(0) + v) / 100
         else {
           (mod(100-x(0),100) - v) / 100
         }

          (next, x(1) + passes)
        })
        .last(1)

    }


  }


  class Day1Test extends AnyFunSuiteLike {

    test("part1 example 1") {
      assert(uk.co.mealor.aoc2025.Day1.part1(
        """L68
          |L30
          |R48
          |L5
          |R60
          |L55
          |L1
          |L99
          |R14
          |L82""".stripMargin.split("\r?\n").toList, 50) === 3)

    }

    test("part1 example 2") {
      assert(uk.co.mealor.aoc2025.Day1.part1(
        """L50
          |L100
          |L200""".stripMargin.split("\r?\n").toList, 50) === 3)

    }

    test("part2 example 1") {
      assert(uk.co.mealor.aoc2025.Day1.part2(
        """L68
          |L30
          |R48
          |L5
          |R60
          |L55
          |L1
          |L99
          |R14
          |L82""".stripMargin.split("\r?\n").toList, 50) === 6)


    }

  }

}