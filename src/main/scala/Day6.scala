import cats.kernel.Semigroup
import zio.UIO

object Day6 extends Day[Long, Long] {
  def part1(in: String) = {
    implicit val c: Semigroup[Set[Char]] = _ union _
    collectAnswers(in)
  }

  def collectAnswers(s: String)(implicit c: Semigroup[Set[Char]]): UIO[Long] = UIO {
    s.split("\n\n").map(_.split("\n").map(_.toSet).reduce(c.combine).size).sum
  }

  def part2(in: String) = {
    implicit val c: Semigroup[Set[Char]] = _ intersect _
    collectAnswers(in)
  }

  val inputs = Map(
    "example" ->
      InputString(
        """abc
          |
          |a
          |b
          |c
          |
          |ab
          |ac
          |
          |a
          |a
          |a
          |a
          |
          |b""".stripMargin),
    "puzzle" -> ResourceInput("day6puzzle.txt")
  )
}
