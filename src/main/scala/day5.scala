package advent

import scala.collection.SortedMap

type BoxStack = SortedMap[Int, List[Char]]

object day5:
  def run() =
    val input = readFile("resources/day5.txt")
    val boxCols =
      input(8).toList.filter(_ != ' ').map(_.toString.toInt).sorted.last
    val boxMap = input.slice(0, 8)
    val colSize = 4
    val boxStacks: BoxStack =
      boxMap.foldLeft(SortedMap.empty[Int, List[Char]])((mapAcc, line) => {
        (0 to boxCols - 1).foldLeft(mapAcc)((innerMapAcc, col) => {
          val charPos = 1 + col * colSize
          val newChar = line.charAt(charPos)
          val mapKey = col + 1

          if (newChar == ' ')
            innerMapAcc
          else
            val newCharList =
                innerMapAcc.getOrElse(mapKey, List.empty) :+ line.charAt(charPos)
            innerMapAcc + (mapKey -> newCharList)
        })
      })

    val moveLines = input.slice(10, input.length)
    val moves = moveLines.map { getMove }

    val newBoxStack = moves.foldLeft(boxStacks)(moveBoxes)
    val newBoxStack9001 = moves.foldLeft(boxStacks)(moveBoxesAtOnce)

    println(newBoxStack.map(_._2.headOption.getOrElse("Nil")).mkString(""))
    println(newBoxStack9001.map(_._2.headOption.getOrElse("Nil")).mkString(""))

  def getMove(moveLine: String) =
    (getIntAtPos(moveLine, 1), getIntAtPos(moveLine, 3), getIntAtPos(moveLine, 5))

  def getIntAtPos(str: String, pos: Int) =
    str.split(" ")(pos).toInt

  def moveBoxes(boxStacks: BoxStack, move: (Int, Int, Int)): BoxStack = move match
    case (number, from, to) =>
      (1 to number).foldLeft(boxStacks)((accStack, _) => {
        val (toAdd, oldList) = accStack(from).splitAt(1)
        val oldStack = (from -> oldList)
        val newStack = (to -> (toAdd ++ accStack(to)))

        accStack + oldStack + newStack
      })

  def moveBoxesAtOnce(boxStacks: BoxStack, move: (Int, Int, Int)): BoxStack = move match
    case (number, from, to) =>
      val (toAdd, oldList) = boxStacks(from).splitAt(number)
      val oldStack = (from -> oldList)
      val newStack = (to -> (toAdd ++ boxStacks(to)))

      boxStacks + oldStack + newStack



