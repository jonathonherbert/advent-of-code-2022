package advent

import scala.io.Source

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

def readFile(path: String) = Source.fromFile(path).getLines.toList
