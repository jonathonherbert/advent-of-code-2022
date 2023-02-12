package advent

import scala.io.Source

val RESET = "\u001B[0m"
val RED = "\u001B[31m"
val GREEN = "\u001B[32m"
val PREV_LINE = "\u001B[1A"
val CLEAR_LINE = "\u001B[2K"
val CLEAR_SCREEN = "\u001b[2J"

def readFile(path: String) = Source.fromFile(path).getLines.toList

def readFileForDay(day: Int) = readFile(s"resources/day${day}.txt")

case class Pixel(str: String | Char, mod: String = "")

def p(p: Pixel) = print(s"${p.mod}${p.str}${RESET}")

type ToPixel[T] = (T, Int, Int) => Pixel

def printGrid[T](grid: List[List[T]], toPixel: ToPixel[T] = (t: T, _: Int, _: Int) => Pixel(t.toString)) =
  grid.zipWithIndex.foreach { line =>
    line._1.zipWithIndex.foreach { item =>
      p(toPixel(item._1, item._2, line._2))
    }
    println
  }

  Thread.sleep(100)

def printGrids[T](grids: List[List[List[T]]], toPixel: ToPixel[T] = (t: T, _: Int, _: Int) => Pixel(t.toString), delay: Int = 1000) =
  grids.foreach { grid =>
    printGrid(grid, toPixel)
    Thread.sleep(delay)
    (0 to grid.length - 1).foreach { (i: Int) => print(PREV_LINE)}
  }

@main def runDay3: Unit =
  day3.run()

@main def runDay4: Unit =
  day4.run()

@main def runDay5: Unit =
  day5.run()

@main def runDay6: Unit =
  day6.run()

@main def runDay7: Unit =
  day7.run()

@main def runDay8: Unit =
  day8.run()

@main def runDay9: Unit =
  day9.run()

@main def runDay10: Unit =
  day10.run()

@main def runDay11: Unit =
  day11.run()

@main def runDay12: Unit =
  day12.run(readFileForDay(12))

@main def runDay13: Unit =
  day13.run(readFileForDay(13))
