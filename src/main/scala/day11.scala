package advent

import scala.annotation.tailrec

type Operator = '*' | '/' | '+' | '-'
val operators: List[Operator] = List('*', '/', '+', '-')
case class Operation(operator: Operator, operand: Option[Long])
case class Test(divisibleBy: Int, ifTrueMonkey: Int, ifFalseMonkey: Int)
case class Item(worryLevel: Long)

case class Monkey(
    id: Int,
    items: List[Item],
    op: Operation,
    test: Test,
    inspections: Int = 0
)

object day11:
  val isDebug = false
  def printV(str: String): Unit = if (isDebug) println(str)

  def getItems(itemStr: String) =
    itemStr
      .replaceAllLiterally("Starting items: ", "")
      .split(", ")
      .map(_.trim)
      .map(_.toLong)
      .map(Item.apply)
      .toList

  def getOperation(opStr: String) =
    opStr
      .replaceAllLiterally("  Operation: new = old ", "")
      .split(" ")
      .toList match {
      case a @ (operator :: operand :: Nil) =>
        (operator.toCharArray.head, operand) match {
          case a: (Operator, (Int | "old")) =>
            Operation(a._1, if (a._2 == "old") None else Some(a._2.toInt))
          case invalidOp => throw Error(s"Invalid operator: ${invalidOp}")
        }
      case opArr => throw Error(s"Invalid operation: ${opArr}")
    }

  def getTest(testStr: String, trueStr: String, falseStr: String) =
    Test(
      testStr.replaceAllLiterally("  Test: divisible by ", "").toInt,
      trueStr.replaceAllLiterally("    If true: throw to monkey ", "").toInt,
      falseStr.replaceAllLiterally("    If false: throw to monkey ", "").toInt
    )

  def run() =
    val input = readFile("resources/day11.txt")

    @tailrec
    def parseInputIntoMonkeys(
        remainingLines: List[String],
        monkeys: List[Monkey] = List.empty
    ): List[Monkey] = remainingLines match {
      case monkeyStr :: itemStr :: opStr :: testStr :: trueStr :: falseStr :: tail =>
        val monkey = Monkey(
          id = monkeyStr
            .replaceAllLiterally("Monkey ", "")
            .replaceAllLiterally(":", "")
            .toInt,
          items = getItems(itemStr),
          op = getOperation(opStr),
          test = getTest(testStr, trueStr, falseStr)
        )
        parseInputIntoMonkeys(tail.drop(1), monkeys :+ monkey)
      case Nil => monkeys
    }

    val monkeys = parseInputIntoMonkeys(input)
      .map(monkey => (monkey.id -> monkey))
      .toMap

    val gcd = monkeys.values.toList.map(_.test.divisibleBy).fold(1)(_ * _)

    println(gcd)

    def applyTurn(monkeys: Map[Int, Monkey], turn: Int) =
      val monkey = monkeys.get(turn).get

      printV(s"Monkey ${monkey.id}: ${monkey.items}")

      monkey.items.foldLeft(monkeys)((acc, item) => {
        printV(s"  Monkey inspects an item with a worry level of ${item}")
        val currentGiver = acc.get(monkey.id).get
        val worryLevelPre = applyOp(item, monkey.op)
        val newWorryLevel = worryLevelPre % gcd
        printV(s"    Worry level ${monkey.op.operator} ${monkey.op.operand.getOrElse("Itself")} = ${worryLevelPre}")
        printV(s"    Monkey gets bored with item. Worry level is divided by 3 to ${newWorryLevel}")
        val testResult = newWorryLevel % monkey.test.divisibleBy == 0
        printV(s"    Current worry level divisible by ${monkey.test.divisibleBy}: ${testResult}")
        val nextMonkeyId = if (testResult) monkey.test.ifTrueMonkey else monkey.test.ifFalseMonkey
        printV(s"    Item with worry le vel ${newWorryLevel} is thrown to monkey ${nextMonkeyId}.")
        val currentReceiver = acc.get(nextMonkeyId).get
        val nextGiver = currentGiver.copy(
          items = currentGiver.items.drop(1),
          inspections = currentGiver.inspections + 1)

        // println(s"Current: ${currentGiver.items}")
        // println(s"After: ${nextGiver.items}")
        val nextReceiver = currentReceiver.copy(items = currentReceiver.items :+ Item(newWorryLevel))

        acc + (nextGiver.id -> nextGiver) + (nextReceiver.id -> nextReceiver)
      })

    def printRound(round: Int, map: Map[Int, Monkey]) =
      println(s"== After round ${round} ==")
      map.foreach((k, v) => println((k -> v.inspections)))

    val afterRounds = (1 to 10000).foldLeft(monkeys)((acc, curr) =>
      val afterRound =  acc.keys.toList.sorted.foldLeft(acc)(applyTurn)
      if (curr == 1 || curr == 20 || curr % 1000 == 0) {
        printRound(curr, afterRound)
        print(acc)
      }
      afterRound
    )

    val businessMap = afterRounds.map((k, v) => (k, v.inspections))

    val top2 = businessMap.values.toList.sorted
      .reverse
      .take(2)

    val total = top2.map(_.toLong).fold(1L)(_ * _)

    println(top2)
    println(total)

  def applyOp(item: Item, op: Operation) = op.operator match {
    case '*' => item.worryLevel * op.operand.getOrElse(item.worryLevel)
    case '/' => item.worryLevel / op.operand.getOrElse(item.worryLevel)
    case '+' => item.worryLevel + op.operand.getOrElse(item.worryLevel)
    case '-' => item.worryLevel - op.operand.getOrElse(item.worryLevel)
  }
