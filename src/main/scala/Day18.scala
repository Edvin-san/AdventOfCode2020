import zio._

object Day18 extends Day[Long, Long] {
  sealed trait Expression
  case class Constant(x: Int) extends Expression
  case class Operator(o: Char) extends Expression
  case class Paren(exprs: List[Expression]) extends Expression

  def parseInput(in: String) = {
    def inner(symbols: List[String], openParens: Int) = symbols match {
      case head :: tail => head match {
        case '(' => ???
        case ')' => ???
        case '+' => ???
        case '*' => ???
        case num => ???
      }
      case Nil => ???
    }
    val cleaned = in.replace("(", " ( ").replace(")", " ) ")
    parseInput(cleaned.split("\s+").toList)
  }

  def part1(in: String) = Task.effect{
    ???
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
