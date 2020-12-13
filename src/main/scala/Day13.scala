import zio._
import ChineseRemainderTheorem.chineseRemainder

object Day13 extends Day[Long, BigInt] {

  def part1(in: String) = Task.effect {
    val lines = in.split("\n")
    val estimatedArrival = lines.head.toLong
    val busIds = lines.last.split(",").filter(_ != "x").map(_.toLong)
    val (bestBusId, departure) = busIds.map { busId =>
      val m = Util.ceilDiv(estimatedArrival, busId)
      busId -> m * busId
    }.sortBy(_._2 - estimatedArrival).head
    bestBusId * (departure - estimatedArrival)
  }

  // Chinese remainder theorem. Assuming all bus ids are coprime (seem to be prime numbers).
  def part2(in: String) = {
    val busIdsWithIndex = in.split("\n").last.split(",").zipWithIndex.filter(_._1 != "x").map(t => (BigInt(t._1), BigInt(t._2))).toList
    val (busIds, remainders) = busIdsWithIndex.map { case (busId, index) => if (index == 0) (busId, index) else (busId, busId - index) }.unzip
    ZIO.succeed(chineseRemainder(busIds, remainders)).someOrFailException
  }

  val inputs = Map(
    "example1" -> InputString(
      """939
        |7,13,x,x,59,x,31,19""".stripMargin),
    "example2" -> InputString(
      """0
        |17,x,13,19""".stripMargin),
    "example3" -> InputString(
      """0
        |67,7,59,61""".stripMargin),
    "example4" -> InputString(
      """0
        |67,x,7,59,61""".stripMargin),
    "example5" -> InputString(
      """0
        |67,7,x,59,61""".stripMargin),
    "example6" -> InputString(
      """0
        |1789,37,47,1889""".stripMargin),
    "puzzle" -> InputString(
      """1000434
        |17,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,983,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,397,x,x,x,x,x,37,x,x,x,x,x,x,13""".stripMargin)
  )
}
