package advent

import scala.util.Try

sealed trait Elem
final case class N(n: Int) extends Elem
final case class L(l: List[Elem] = List.empty) extends Elem

enum TokenType:
  case LEFT_BRACKET, RIGHT_BRACKET, NUMBER, EOL

case class Token(
  tokenType: TokenType,
  lexeme: String,
  literal: Int | Null
):
  override def toString = s"${tokenType} ${lexeme} ${literal}"

object day13:
  def run(lines: List[String]) =
    val elemPairs = lines.grouped(3).foldLeft(List.empty[(L, L)])((currElems, line) => line match
      case firstPacket :: secondPacket :: _ => currElems :+ elemFromStr(firstPacket) -> elemFromStr(secondPacket)
    )

    val pairResults = elemPairs.map {
      case (left, right) => compareElems(left, right)
    }

    pairResults.zipWithIndex.foreach {
      case (result, zeroIdx) =>
        println(s"Index ${zeroIdx + 1}: ${result}")
    }

  var iter = 0
  def compareElems(left: L, right: L): Boolean =
    if (iter > 1000)
      println("Too many iterations")
      false
    else
      iter = iter + 1
      println(s"Compare ${left} with ${right}")
      left.l.zipAll(right.l, L(), L()).forall {
        case (N(lNum), N(rNum)) =>
          if (lNum <= rNum) true else false
        case (_, L(Nil)) => false
        case (L(Nil), _) => true
        case (L(l), N(r)) =>
          println(s"${l}, ${r}")
          compareElems(L(l), L(List(N(r))))
        case (N(l), L(r)) => compareElems(L(List(N(l))), L(r))
        case (L(l), L(r)) => compareElems(L(l), L(r))
      }

  def elemFromStr(str: String): L =
    val tokens = scan(str)
    val l = new Parser(tokens).parse()
    l

  def scan(str: String, curTokens: List[Token] = List.empty): List[Token] =
    str.headOption.match {
      case None => curTokens :+ Token(TokenType.EOL, "", null)
      case Some('[') => (curTokens :+ Token(TokenType.LEFT_BRACKET, "[", null)) ++ scan(str.tail)
      case Some(']') => (curTokens :+ Token(TokenType.RIGHT_BRACKET, "]", null)) ++ scan(stripLeadingComma(str.tail))
      case Some(char) =>
        val numberStr = str.takeWhile(char => Try(char.toString.toInt).isSuccess)
        val token = Token(TokenType.NUMBER, numberStr, numberStr.toInt)
        val remainingStr = str.slice(numberStr.length, str.length)
        (curTokens :+ token) ++ scan(stripLeadingComma(remainingStr))
    }

  def stripLeadingComma(str: String) =
    if (str.headOption.exists(_ == ',')) str.tail else str

class Parser(val tokens: List[Token]):
  var current: Int = 0;

  // Grammar:
  // elem -> list
  // list -> "[" elem ("," elem) "]"
  // elem -> (number | list)*
  def parse(): L =
    elem()

  def elem(): L =
    list()

  def list(): L =
    consume(TokenType.LEFT_BRACKET, "Expected LB")
    var literals = List.empty[Elem]
    while (peek().tokenType != TokenType.RIGHT_BRACKET && !isAtEnd)
      literals = literals :+ literal()
    consume(TokenType.RIGHT_BRACKET, "Expected RB")

    L(literals)

  def literal(): L | N =
    peek().tokenType match {
      case TokenType.LEFT_BRACKET => list()
      case TokenType.NUMBER => number()
    }

  def number(): N =
    consume(TokenType.NUMBER, "Expected number")
    val lit = previous().literal
    lit match {
      case l: Int => N(l)
      case _ => throw new Error(s"Expected number, got ${lit}")
    }

  def matchTokens(tokens: TokenType*) =
  tokens.exists(token =>
    if (check(token)) {
      advance()
      true
    } else false
  )

  def check(tokenType: TokenType) =
    if (isAtEnd) false else peek().tokenType == tokenType

  def isAtEnd = peek().tokenType == TokenType.EOL

  def peek() = tokens(current)

  def advance() =
    if (!isAtEnd) current = current + 1
    previous()

  def consume(tokenType: TokenType, message: String) = {
    if (check(tokenType)) advance()
    else throw new Error(s"Could not consume ${tokenType}: ${peek().tokenType}")
  }

  def previous() = tokens(current - 1)





