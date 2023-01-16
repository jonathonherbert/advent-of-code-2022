package advent

case class Move(val dir: 'U' | 'D' | 'L' | 'R', val spaces: Int)
case class Coord(x: Int, y: Int)

object day9:
  def run() =
    val input = readFile("resources/day9.txt")
    val steps = input.map(line => {
      (line.charAt(0), line.slice(2, line.length).toString.toInt) match {
        case ('U', spaces) => Move('U', spaces)
        case ('D', spaces) => Move('D', spaces)
        case ('L', spaces) => Move('L', spaces)
        case ('R', spaces) => Move('R', spaces)
      }
    })

    // println(steps)

    val segments = List.fill(10)(Coord(0, 0))
    var tailPositions = Set.empty[Coord]

    // printGrid(head, tail)

    val (finalSegments, finalTailPositions) =
      steps.foldLeft((segments, tailPositions))(
        (acc, step) => {
          var (newSegments, newTailPositions) = acc

         //  println(s"\n == ${step.dir} ${step.spaces} == \n")

          (1 to step.spaces).foreach((i: Int) => {
            newSegments.zipWithIndex.foreach { (segment, index) =>
              val newSegment = if (index == 0)
                moveHead(newSegments(0), step)
              else
                moveTail(newSegments(index - 1), newSegments(index))
              newSegments = newSegments.updated(index, newSegment)
            }

            newTailPositions = newTailPositions + newSegments.last

            // printGrid(newSegments)
          })

          // print(step)

          (newSegments, newTailPositions)
        }
      )

    val allTailPos = finalTailPositions.map(t => (t.x -> t.y)).toSet
    println(allTailPos.size)

    printStarts(finalTailPositions.toList)

  def moveHead(head: Coord, step: Move): Coord = step.dir match {
    case 'U' => head.copy(y = head.y + 1)
    case 'D' => head.copy(y = head.y - 1)
    case 'L' => head.copy(x = head.x - 1)
    case 'R' => head.copy(x = head.x + 1)
  }

  def moveTail(head: Coord, tail: Coord): Coord =
    val xDiff = head.x - tail.x
    val yDiff = head.y - tail.y

    (xDiff, yDiff) match {
      case _ if (xDiff == 0 && yDiff == 0) => tail
      // Differs on x only
      case _ if (yDiff == 0) =>
        // No need to move
        if (xDiff.abs == 1) tail
        else tail.copy(x = tail.x + clampOne(xDiff))
      // Differs on y only
      case _ if (xDiff == 0) =>
        if (yDiff.abs == 1) tail
        else tail.copy(y = tail.y + clampOne(yDiff))
      // Differs on x and y
      case _ =>
        if (xDiff.abs < 2 && xDiff.abs == yDiff.abs) tail
        else tail.copy(x = tail.x + clampOne(xDiff), y = tail.y + clampOne(yDiff))
    }

  def clampOne(x: Int) = clamp(x, -1, 1)

  def clamp(x: Int, low: Int, high: Int) = low max x min high

  def printGrid(segments: List[Coord], maxX: Int = 30, maxY: Int = 15, reset: Boolean = true) =

    segments.map(_.x).max
    val upperX = segments.map(_.x).max.max(maxX)
    val lowerX = segments.map(_.x).min.min(-maxX)
    val upperY = segments.map(_.y).max.max(maxY)
    val lowerY = segments.map(_.y).min.min(-maxY)

    // println(s"head: ${(head.x, head.y)}, tail: ${(tail.x, tail.y)}")

    (upperY to lowerY by -1).foreach { y =>
      (lowerX to upperX).foreach { x =>
        val segmentIdx = segments.indexOf(Coord(x, y))

        if (segmentIdx != -1) print(if (segmentIdx == 0) "H" else segmentIdx)
        else print(".")
      }
      print("\n")
    }

    print("\n")

  def printStarts(segments: List[Coord], maxX: Int = 15, maxY: Int = 15) =
    val upperX = segments.map(_.x).max.max(maxX)
    val lowerX = segments.map(_.x).min.min(-maxX)
    val upperY = segments.map(_.y).max.max(maxY)
    val lowerY = segments.map(_.y).min.min(-maxY)
    val tailsSet = segments.toSet

    (upperY to lowerY by -1).foreach { y =>
      (lowerX to upperX).foreach { x =>
        if (x == 0 && y == 0) {
          print(GREEN)
          print("s")
          print(RESET)
        } else if (tailsSet.contains(Coord(x, y))) print("#")
        else print(".")
      }
      print("\n")
    }

    print("\n")
