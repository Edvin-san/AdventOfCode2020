import zio._

import scala.annotation.tailrec

object Day25 extends Day[Long, Long] {
  @tailrec
  def sim(cur: Long, loopSize: Long, toFind: (Long, Long)): Either[Long, Long] =
    if (cur == toFind._1) Left(loopSize)
    else if (cur == toFind._2) Right(loopSize)
    else sim((cur * 7) % 20201227, loopSize + 1, toFind)

  def part1(in: String) = Task.effect{
    val pubKeys = in.split("\n").map(_.toLong).toList
    val priv = sim(1, 0, (pubKeys(0), pubKeys(1)))
    println(priv)
    -1
  }

  def part2(in: String) = Task.effect{
    ???
  }

  val inputs = Map(
    "example" -> InputString(
      """5764801
        |17807724
        |""".stripMargin),
    "puzzle" -> InputString( // first private key == 538014
      """12578151
        |5051300
        |""".stripMargin)
  )
}
