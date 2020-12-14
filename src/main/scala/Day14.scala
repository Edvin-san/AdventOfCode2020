import zio._

object Day14 extends Day[Long, Long] {
  sealed trait Instruction
  case class SetMask(value: String) extends Instruction
  case class WriteToAddress(address: Long, value: Long) extends Instruction

  def parseInput(s: String): List[Instruction] = s.split("\n").map { line =>
    if (line.startsWith("mask")) SetMask(line.drop(7))
    else {
      val longs = line.drop(4).split("] = ")
      WriteToAddress(longs(0).toLong, longs(1).toLong)
    }
  }.toList

  def part1(in: String) = Task.effect{
    val all1s = 0xFFFFFFFFFL
    val all0s = 0L
    var andMask: Long = all1s
    var orMask: Long = all0s
    var mem: Map[Long, Long] = Map()

    for(instr <- parseInput(in)) {
      instr match {
        case SetMask(value) =>
          andMask = value.replace('X', '1').toLong // TODO want Integer.parseInt(x, 2) but for long
          orMask = value.replace('X', '0').toLong
        case WriteToAddress(address, value) =>
      }
    }
    ???
  }

  def part2(in: String) = Task.effect{
    ???
  }

  val inputs = Map(
    "example" -> InputString("""mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
                               |mem[8] = 11
                               |mem[7] = 101
                               |mem[8] = 0""".stripMargin),
    "puzzle" -> ResourceInput("day14puzzle.txt")
  )
}
