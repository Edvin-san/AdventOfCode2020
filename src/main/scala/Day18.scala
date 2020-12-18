import zio._

object Day18 extends Day[Long, Long] {
  sealed trait Expression
  case class Constant(x: Int) extends Expression
  case class Operator(o: Char) extends Expression
  case class Paren(exprs: List[Expression]) extends Expression

  def parseLine(in: String): Paren = {
    println(s"Trying to parse $in")
    def inner(symbols: List[String], acc: List[Expression]): (List[Expression], List[String]) = symbols match {
      case head :: tail => head match {
        case "("=>
          val (inParen, remaining) = inner(tail, List())
          val (restInMyLevel, remainingAfterMyLevel) = inner(remaining, acc.appended(Paren(inParen)))
          (restInMyLevel, remainingAfterMyLevel)
        case ")" => (acc, tail)
        case "+" => inner(tail, acc.appended(Operator('+')))
        case "*" => inner(tail, acc.appended(Operator('*')))
        case num => inner(tail, acc.appended(Constant(num.toInt)))
      }
      case Nil => (acc, Nil)
    }
    val cleaned = in.replace("(", " ( ").replace(")", " ) ")
    val tokenized = cleaned.split("\\s+").toList
    println(s"tokenized: $tokenized")
    val (exprs, leftover) = inner(tokenized, Nil)
    assert(leftover == Nil)
    println(s"Parsed $in to ${Paren(exprs)}")
    Paren(exprs)
  }

  def part1(in: String) = Task.effect{
    println(in)
    val exprs = in.split("\n").filter(!_.isEmpty).map(parseLine)
    -1
  }

  def part2(in: String) = Task.effect{
    ???
  }

  val inputs = Map(
    "example" -> InputString("1 + 2 * 3 + 4 * 5 + 6"),
    "example2" -> InputString("1 + (2 * 3) + (4 * (5 + 6))"),
    "example3" -> InputString("2 * 3 + (4 * 5)"),
    "example4" -> InputString("5 + (8 * 3 + 9 + 3 * 4 * 3)"),
    "example5" -> InputString("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"),
    "example6" -> InputString("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"),
    "puzzle" -> ResourceInput("day18puzzle.txt")
  )
}
