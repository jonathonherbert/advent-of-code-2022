package advent

object day6:
  def run() =
    val input = readFile("resources/day6.txt").head.toCharArray().toList

    def collectPos(input: List[Char], lengthOfUniques: Int, buffer: List[Char] = List.empty, pos: Int = 0): Int =
      input match {
        case Nil => 0
        case head :: tail if buffer.toSet.size == lengthOfUniques =>
          pos
        case head :: tail =>
          val newBuffer = buffer match {
            case Nil => List(head)
            case b => b.takeRight(lengthOfUniques - 1) :+ head
          }
          collectPos(tail, lengthOfUniques, newBuffer, pos + 1)
      }

    println(collectPos(input, 4))
    println(collectPos(input, 14))
