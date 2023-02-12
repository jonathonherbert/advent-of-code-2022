package advent

import scala.annotation.tailrec

case class Vec(x: Int, y: Int)
case class Pos(char: Char, distance: Option[Int] = None)
case class Mv(dir: Char, pos: Vec)
type Grid = List[List[Pos]]

object day12:
  val debug = false

  def p = println()
  def p(out: Any) = if (debug) println(out)

  def posIsWithinBounds(pos: Vec, grid: Grid): Boolean =
    val maxY = grid.length - 1
    val maxX = grid(0).length - 1
    pos.x >= 0 && pos.x <= maxX &&
    pos.y >= 0 && pos.y <= maxY

  def getCharForPos(pos: Vec, grid: Grid) =
    val char = grid(pos.y)(pos.x).char
    if (char == 'E') 'z'
    else if (char == 'S') 'a'
    else char

  def getDistance(pos1: Vec, pos2: Vec) =
    (pos1.x - pos2.x).abs + (pos1.y - pos2.y).abs

  def getValidMoves(
      vec: Vec,
      distance: Int,
      grid: Grid
  ) =
    val curPos = grid(vec.y)(vec.x)
    val left = Mv('<', vec.copy(x = vec.x - 1))
    val right = Mv('>', vec.copy(x = vec.x + 1))
    val up = Mv('^', vec.copy(y = vec.y - 1))
    val down = Mv('v', vec.copy(y = vec.y + 1))

    val curChar = getCharForPos(
      vec,
      grid
    )

    val validMoves = List(left, right, up, down).filter(move =>
      if (!posIsWithinBounds(move.pos, grid)) {
        p(s"Cannot move ${move.pos} – out of bounds")
        false
      } else {
        val newPos = grid(move.pos.y)(move.pos.x)
        val moveChar = getCharForPos(move.pos, grid)

        val moveResultsInLowerScore = (!newPos.distance.isDefined || newPos.distance.get > curPos.distance.getOrElse(0) + 1)
        val isValidMove = moveChar.toInt <= curChar.toInt + 1 && moveResultsInLowerScore

        // if (vec.x == 1 && vec.y == 28) println(
        //   s"From ${curChar} (${(vec.x, vec.y, curPos.distance.getOrElse(-1))}) to ${moveChar} (${(move.pos.x, move.pos.y, newPos.distance.getOrElse(-1))}) (${curChar.toInt} to ${moveChar.toInt}): ${isValidMove} because ${(moveChar.toInt <= curChar.toInt + 1, newPos.distance.isDefined, newPos.distance.getOrElse(0) > curPos.distance
        //     .getOrElse(0) + 1)}"
        // )

        isValidMove
      }
    )

    validMoves

  def run(lines: List[String]) =
    val start = Vec(0, 20)
    val grid = replaceAtPos(
      start.x,
      start.y,
      Pos('S', Some(0)),
      lines.map(_.toList.map(char => Pos(char)))
    )

    def loop(grid: Grid, pos: Vec = start): Grid =
      val curPos = grid(pos.y)(pos.x)
      val validMoves =
        getValidMoves(pos, curPos.distance.getOrElse(Int.MaxValue), grid)
      validMoves.foldLeft(grid)((grid, move) => {
        val newPos = grid(move.pos.y)(move.pos.x)
        val newGrid = replaceAtPos(
          move.pos.x,
          move.pos.y,
          newPos.copy(distance = Some(curPos.distance.getOrElse(0) + 1)),
          grid
        )
        loop(newGrid, move.pos)
      })

    val allAs = grid.zipWithIndex.flatMap { case (line, y) => line.zipWithIndex.flatMap {
      case (pos, x) => if (pos.char == 'a') Some((pos, x, y)) else None } }

    println(allAs)

    val allDistances = allAs.flatMap { start =>
      val newGrid = loop(grid, Vec(start._2, start._3))
      val endDistance = findEndDistance(newGrid)
      println(s"Done ${start}")
      endDistance.headOption.flatten
    }

    println(allDistances)

  def findEndDistance(grid: Grid) =
    grid.flatMap { _.flatMap { pos => if (pos.char == 'E') Some(pos.distance) else None } }

  def pGrid(g: Grid) = printGrid(
    g,
    (p, _, _) => Pixel((p.distance.getOrElse(0) / 10).toString.padTo(2, ' '))
  )

  def replaceAtPos(x: Int, y: Int, pos: Pos, grid: List[List[Pos]]) =
    grid.updated(y, grid(y).updated(x, pos))

  def printGridWithMoves(grid: Grid, moves: List[Mv]) =
    printGrid(
      grid,
      (char, x, y) =>
        val pos = Vec(x, y)
        moves.indexWhere(move => move.pos == pos) match {
          case -1                            => Pixel(grid(y)(x).char)
          case 0                             => Pixel('S')
          case idx if idx < moves.length - 1 => Pixel(moves(idx + 1).dir)
          case _                             => Pixel('X')
        }
    )
