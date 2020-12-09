import zio._

import scala.annotation.tailrec

object Day9 extends Day[Long, Long] {

  // Only realised I could use prefix sum for part 2.
  def makePrefixSum(nums: List[Long]): Array[Long] = nums
    .foldLeft(List(0L))((prev, n) => prev.appended(prev.last + n))
    .toArray

  // Last value is not the sum of any of the previous.
  case class InvalidSequence(nums: List[Long])

  case class ValidSequence(nums: List[Long]) {
    def isValidNext(i: Long): Boolean = nums.combinations(2).map(_.reduce(_ + _)).contains(i)

    def next(i: Long): Either[InvalidSequence, ValidSequence] =
      Either.cond(isValidNext(i), ValidSequence(nums.tail.appended(i)), InvalidSequence(nums.appended(i)))
  }

  object ValidSequence {
    def make(preamble: List[Long]): ValidSequence = ValidSequence(preamble)
  }

  def parse(s: String): (ValidSequence, List[Long]) = {
    val lines = s.split("\n")
    val preambleLen = lines.head.toInt
    val preamble = lines.tail.take(preambleLen).map(_.toLong)
    val rest = lines.drop(preambleLen + 1).map(_.toLong)
    (ValidSequence.make(preamble.toList), rest.toList)
  }

  @tailrec
  def findInvalidSequence(sequence: ValidSequence, rest: List[Long]): Option[InvalidSequence] = rest match {
    case head :: tail => sequence.next(head) match {
      case Left(invalidSequence) => Some(invalidSequence)
      case Right(validSequence) => findInvalidSequence(validSequence, tail)
    }
    case Nil => None
  }

  def part1(in: String) = Task.effect {
    (findInvalidSequence _).tupled(parse(in)).get.nums.last
  }

  def part2(in: String) = {
    val (sequence, rest) = parse(in)
    val target = findInvalidSequence(sequence, rest).get.nums.last
    val all = sequence.nums ++ rest
    val prefixSum = makePrefixSum(all)
    val ranges = for {
      s <- 0 until all.size
      e <- s + 2 to all.size
    } yield (s, e)
    val checks = ranges.map {
      case (start, end) =>
        if (prefixSum(end) - prefixSum(start) == target) {
          for {
//            _ <- console.putStrLn(s"$all")
//            _ <- console.putStrLn(s"${prefixSum.toList}")
//            _ <- console.putStrLn(s"Found that ${prefixSum(end)} - ${prefixSum(start)} == $target")
//            _ <- console.putStrLn(s"Indices: $start $end in prefix sum array")
            _ <- ZIO.unit
            slice = all.slice(start, end)
//            _ <- console.putStrLn(s"slice: ${slice}.sum == ${slice.sum} =? $target")
          } yield (slice.min, slice.max)
        } else
          ZIO.fail(new Throwable("No sum found!"))
    }
    for {
      (a, b) <- ZIO.firstSuccessOf(checks.head, checks.tail)
    } yield a + b
  }

  /*
    3 6 2
sum 0 3 9 11
ix  0 1 2 3
  */

  // Added [preamble] to inputs (example preamble 5), puzzle 25
  val inputs = Map(
    "example" -> InputString(
      """5
        |35
        |20
        |15
        |25
        |47
        |40
        |62
        |55
        |65
        |95
        |102
        |117
        |150
        |182
        |127
        |219
        |299
        |277
        |309
        |576""".stripMargin),
    "puzzle" -> ResourceInput("day9puzzle.txt")
  )
}
