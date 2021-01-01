import zio._

import scala.annotation.tailrec

object Day22 extends Day[BigInt, BigInt] {
  def parseInput(in: String): (List[Int], List[Int]) = in.split("\n\n").map(_.split("\n").drop(1).map(_.toInt).toList) match {
    case Array(p1, p2) => (p1, p2)
  }

  @tailrec
  def simPart1(p: (List[Int], List[Int])): (List[Int], List[Int]) = p match {
    case (_ :: _, Nil) => p
    case (Nil, _ :: _) => p
    case (h1 :: t1, h2 :: t2) =>
      if (h1 > h2)
        simPart1((t1 ::: List(h1, h2), t2))
      else
        simPart1((t1, t2 ::: List(h2, h1)))
  }

  case class GameOutcome(player1Won: Boolean, finalConfiguration: (List[Int], List[Int]))

  def simPart2(p: (List[Int], List[Int]), previousRounds: Set[(List[Int], List[Int])]): GameOutcome = {
    p match {
      case (_ :: _, Nil) => GameOutcome(true, p)
      case (Nil, _ :: _) => GameOutcome(false, p)
      case (h1 :: t1, h2 :: t2) =>
        if (previousRounds.contains(p)) GameOutcome(true, p)
        else {
          val p1WinsRound =
            if (h1 <= t1.size && h2 <= t2.size) simPart2((t1.take(h1), t2.take(h2)), Set()).player1Won
            else h1 > h2
          if (p1WinsRound)
            simPart2((t1 ::: List(h1, h2), t2), previousRounds + p)
          else
            simPart2((t1, t2 ::: List(h2, h1)), previousRounds + p)
        }
    }
  }

  def part1(in: String) = Task.effect {
    val initial = parseInput(in)
    val (f1, f2) = simPart1(initial)
    val winner = List(f1, f2).find(!_.isEmpty).get
    winner.map(BigInt(_)).reverse.zipWithIndex.map(t => t._1 * (t._2 + 1)).sum
  }

  def part2(in: String) = Task.effect {
    val initial = parseInput(in)
    val GameOutcome(p1Won, (f1, f2)) = simPart2(initial, Set())
    val winner = if (p1Won) f1 else f2
    winner.map(BigInt(_)).reverse.zipWithIndex.map(t => t._1 * (t._2 + 1)).sum
  }

  val inputs = Map(
    "example" -> InputString(
      """Player 1:
        |9
        |2
        |6
        |3
        |1
        |
        |Player 2:
        |5
        |8
        |4
        |7
        |10""".stripMargin),
//    "infinite" -> InputString(
//      """Player 1:
//        |43
//        |19
//        |
//        |Player 2:
//        |2
//        |29
//        |14""".stripMargin),
    "puzzle" -> InputString(
      """Player 1:
        |41
        |26
        |29
        |11
        |50
        |38
        |42
        |20
        |13
        |9
        |40
        |43
        |10
        |24
        |35
        |30
        |23
        |15
        |31
        |48
        |27
        |44
        |16
        |12
        |14
        |
        |Player 2:
        |18
        |6
        |32
        |37
        |25
        |21
        |33
        |28
        |7
        |8
        |45
        |46
        |49
        |5
        |19
        |2
        |39
        |4
        |17
        |3
        |22
        |1
        |34
        |36
        |47""".stripMargin)
  )
}
