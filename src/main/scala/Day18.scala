import zio._

object Day18 extends Day[Long, Long] {

  sealed trait Expression {
    def wrappedInParen: Paren = this match {
      case c: Constant => Paren(List(c))
      case p: Paren => p
      case o: Operator => ???
    }
    def prettyPrint: String = this match {
      case Constant(x) => x.toString
      case Operator(o) => o.toString
      case Paren(List(e)) => e.prettyPrint
      case Paren(exprs) => "(" + exprs.map(_.prettyPrint).mkString(" ") + ")"
    }
  }
  case class Constant(x: Long) extends Expression
  case class Operator(o: Char) extends Expression {
    def combine(a: Constant, b: Constant): Constant = this.o match {
      case '+' => Constant(a.x + b.x)
      case '*' => Constant(a.x * b.x)
    }
  }
  case class Paren(exprs: List[Expression]) extends Expression

  def parseLine(in: String): Paren = {
    def inner(symbols: List[String], acc: List[Expression]): (List[Expression], List[String]) = symbols match {
      case head :: tail => head match {
        case "(" =>
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
    val tokenized = cleaned.trim.split("\\s+").toList
    val (exprs, leftover) = inner(tokenized, Nil)
    assert(leftover == Nil)
    Paren(exprs)
  }

  def eval(expr: Paren): Long = expr.exprs match {
    case Constant(x) :: Nil => x
    case Paren(ps) :: Nil => eval(Paren(ps))
    case x :: Operator(c) :: y :: tail => eval(Paren(List(Operator(c).combine(Constant(eval(x.wrappedInParen)), Constant(eval(y.wrappedInParen)))) ++ tail))
  }

  def part1(in: String) = Task.effect {
    val exprs = in.split("\n").filter(!_.isEmpty).map(parseLine).toList
    exprs.map(expr => eval(expr)).sum
  }

  def adapt(expr: Paren): Paren = expr.exprs match {
    case Constant(x) :: Nil => Constant(x).wrappedInParen
    case Paren(ps) :: Nil => adapt(Paren(ps))
    case x :: Operator('+') :: y :: Operator(c) :: tail => adapt(Paren(
      List(
        Paren(
          List(
            adapt(x.wrappedInParen),
            Operator('+'),
            adapt(y.wrappedInParen))),
        Operator(c),
        ) ++ tail))
    case x :: Operator('+') :: y :: Nil =>
        Paren(
          List(
            adapt(x.wrappedInParen),
            Operator('+'),
            adapt(y.wrappedInParen))
        )
    case x :: Operator('*') :: tail => Paren(
      List(adapt(x.wrappedInParen), Operator('*'), adapt(Paren(tail)))
    )
  }

  def part2Debug(in: String) = Task.effect {
    val base = parseLine(in)
    val advanced = adapt(base)
    println(s"$in === \n${base.prettyPrint} === \n${advanced.prettyPrint}")
    eval(advanced)
  }

  def part2(in: String) = Task.effect {
    val exprs = in.split("\n").filter(!_.isEmpty).map(parseLine).map(adapt).toList
    exprs.map(expr => eval(expr)).sum
  }

  val inputs = Map(
    "example" -> InputString("1 + 2 * 3 + 4 * 5 + 6"),
    "example2" -> InputString("1 + (2 * 3) + (4 * (5 + 6))"),
    "example3" -> InputString("2 * 3 + (4 * 5)"),
    "example4" -> InputString("5 + (8 * 3 + 9 + 3 * 4 * 3)"),
    "example5" -> InputString("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"),
    "example6" -> InputString("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"),
    "puzzle" -> ResourceInput("day18puzzle.txt") // Part2 362880372308125
  )
}
