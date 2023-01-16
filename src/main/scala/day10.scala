package advent

case class Noop()
case class AddX(v: Int)
type Instruction = Noop | AddX
case class Tick(i: Instruction)

class Processor:
  var X = 1

  def runTick(t: Tick) = t match {
    case Tick(Noop()) =>
      ()
    case Tick(AddX(v)) =>
      X = X + v
      ()
  }

object day10:
  def run() =
    val input = readFile("resources/day10.txt")
    val instructions = input.map(getInstructionFromLine)
    val ticks = getTicksFromInstructions(instructions)
    val proc = new Processor
    var out = List.empty[Char]

    ticks.zipWithIndex.foreach((tick, idx) => {
      val iter = idx + 1
      val linePos = idx % 40
      val onSprite = (proc.X - 1) <= linePos && linePos <= (proc.X + 1)

      val char = if (onSprite) '#' else '.'
      out = out :+ char

      proc.runTick(tick)
    })

    printOut(out)

  def printOut(l: List[Char]) =
    l.zipWithIndex.foreach((c, idx) => {
      if (idx % 40 == 0) print("\n")
      print(c)

    })

  def getTicksFromInstructions(i: List[Instruction]) =
    i.foldLeft(List.empty[Tick])((acc, curr) =>
      curr match {
        case Noop()    => acc :+ Tick(Noop())
        case add: AddX => acc :+ Tick(Noop()) :+ Tick(add)
      }
    )

  def getInstructionFromLine(line: String): Instruction =
    line.take(4) match {
      case "noop" => Noop()
      case "addx" =>
        val Array(iStr, valStr) = line.split(" ")
        AddX(valStr.toInt)
    }
