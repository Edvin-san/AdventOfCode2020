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

  def part1(in: String) = Task.effect {
    val all1s = 0xFFFFFFFFFL
    val all0s = 0L
    var andMask: Long = all1s
    var orMask: Long = all0s
    var mem: Map[Long, Long] = Map()

    for (instr <- parseInput(in)) {
      instr match {
        case SetMask(value) =>
          andMask = java.lang.Long.parseLong(value.replace('X', '1'), 2)
          orMask = java.lang.Long.parseLong(value.replace('X', '0'), 2)
        case WriteToAddress(address, value) =>
          val x = (value & andMask) | orMask
          mem = mem.updated(address, x)
      }
    }
    mem.values.sum
  }

  def getMemoryAddresses(floatingAddress: String): List[Long] = {
    def inner(ss: String): List[String] = (ss.headOption, ss.drop(1)) match {
      case (None, _) => List("")
      case (Some(head), tail) => head match {
        case '1' => inner(tail).map(x => "1" + x)
        case '0' => inner(tail).map(x => "0" + x)
        case 'X' => inner("1" + ss.drop(1)) ++ inner("0" + ss.drop(1))
      }
    }

    inner(floatingAddress).map(x => java.lang.Long.parseLong(x, 2))
  }

  /*
  1X0 = 2
  0X1 = 2
  XXX = 8
   */

  def part2(in: String) = Task.effect {
    var mask = ""
    var mem: Map[Long, Long] = Map()
    val instrs = parseInput(in)
    if (instrs.collect { case SetMask(mask) => mask.count(_ == 'X') }.max > 12) -1
    else {
      for (instr <- instrs) {
        instr match {
          case SetMask(value) =>
            mask = value
          case WriteToAddress(address, value) =>
            val floatingAddress = mask.zip(address.toBinaryString.reverse.padTo(36, '0').reverse).map {
              case ('0', x) => x
              case ('1', _) => '1'
              case ('X', _) => 'X'
            }.mkString
            mem = mem ++ getMemoryAddresses(floatingAddress).map(x => (x, value)).toMap
        }
      }
      mem.values.sum
    }
  }

  val inputs = Map(
    "example" -> InputString(
      """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
        |mem[8] = 11
        |mem[7] = 101
        |mem[8] = 0""".stripMargin),
    "example2" -> InputString(
      """mask = 000000000000000000000000000000X1001X
        |mem[42] = 100
        |mask = 00000000000000000000000000000000X0XX
        |mem[26] = 1""".stripMargin),
    "puzzle" -> ResourceInput("day14puzzle.txt")
  )
}
