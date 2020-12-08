import Day8.Machine.{Acc, I, Instruction, InvalidProgramCounter, Jmp, Nop, ProgramState}
import zio._
import zio.duration.durationInt

import scala.util.{Failure, Success, Try}

object Day8 extends Day[Long, Long] {

  object Machine {
    type I = Int

    sealed trait Instruction

    case class Acc(i: I) extends Instruction

    case class Jmp(i: I) extends Instruction

    case class Nop(i: I) extends Instruction

    case class ProgramState(pc: I, instructions: Map[I, Instruction], acc: I)

    def parseInstructions(s: String): Try[Map[I, Instruction]] = Try {
      s.split("\n").map(_.split(" ") match {
        case Array("nop", i) => Nop(i.toInt)
        case Array("acc", i) => Acc(i.toInt)
        case Array("jmp", i) => Jmp(i.toInt)
        case huh => throw new IllegalArgumentException(s"Unexpected instruction: $huh")
      }).zipWithIndex.map(_.swap).toMap
    }

    case class InvalidProgramCounter(state: ProgramState) extends Throwable

  }

  def interpret[R](state: ProgramState, halt: ProgramState => URIO[R, Boolean]): ZIO[R, InvalidProgramCounter, ProgramState] = halt(state).flatMap {
    case true => ZIO.succeed(state)
    case false => state.instructions.get(state.pc) match {
      case None => ZIO.fail(InvalidProgramCounter(state))
      case Some(instr) => instr match {
        case Machine.Acc(i) => interpret(state.copy(pc = state.pc + 1, acc = state.acc + i), halt)
        case Machine.Jmp(i) => interpret(state.copy(pc = state.pc + i), halt)
        case Machine.Nop(_) => interpret(state.copy(pc = state.pc + 1), halt)
      }
    }
  }

  implicit class ProgramStateOps(state: ProgramState) {
    private def signedI(i: I): String = if (i >= 0) s"+$i" else s"$i"

    def prettyPrint(visited: Set[I]): String = {
      def v(i: I) = if (visited.contains(i)) "*" else " "
      s"acc: ${state.acc}\n" + state.instructions.toList.sortBy(_._1).map {
        case (index, instr) => (if (index == state.pc) "--->" else "    ") + v(index) + (instr match {
          case Acc(i) => s"acc ${signedI(i)}"
          case Jmp(i) => s"jmp ${signedI(i)}"
          case Nop(i) => s"nop ${signedI(i)}"
        })
      }.mkString("\n")
    }
  }

  def findLoop(initialState: ProgramState) = for {
    visited <- Ref.make[Set[Int]](Set())
    halt = (state: ProgramState) => for {
      seen <- visited.get
      _ <- visited.update(_ + state.pc)
    } yield seen.contains(state.pc)
    finalState <- interpret(initialState, halt)
  } yield finalState

  def part1(in: String) = {
    for {
      instrs <- ZIO.fromTry(Machine.parseInstructions(in))
      prog = ProgramState(0, instrs, 0)
      finalState <- findLoop(prog)
    } yield finalState.acc
  }

  def part2(in: String) = {
    def flip(instr: Instruction): Try[Instruction] = instr match {
      case Acc(_) => Failure(new IllegalArgumentException("Can't flip acc"))
      case Jmp(i) => Success(Nop(i))
      case Nop(i) => Success(Jmp(i))
    }

    def flipIth(instrs: Map[Int, Instruction])(idx: Int): Try[Map[Int, Instruction]] =
      for {
        instr <- Try(instrs(idx))
        flipped <- flip(instr)
      } yield instrs.updated(idx, flipped)

    def findJmpsOrNops(instrs: Map[Int, Instruction]): Array[Int] = instrs.toArray.filter {
      case (_, instr) => instr match {
        case Acc(_) => false
        case Jmp(_) => true
        case Nop(_) => true
      }
    }.map(_._1).sorted

    sealed trait Outcome
    case class InfiniteLoop(acc: Int) extends Outcome
    case class FinishAfterLastOp(acc: Int) extends Outcome
    case class InvalidPC(acc: Int) extends Outcome

    def determineOutcome(initialState: ProgramState): UIO[Outcome] = findLoop(initialState).either.map {
      case Left(InvalidProgramCounter(state)) if state.pc == state.instructions.size => FinishAfterLastOp(state.acc)
      case Left(InvalidProgramCounter(state)) => InvalidPC(state.acc)
      case Right(state) => InfiniteLoop(state.acc)
    }

    for {
      instrs <- ZIO.fromTry(Machine.parseInstructions(in))
      jmpsOrNops = findJmpsOrNops(instrs)
      flip = flipIth(instrs) _
      programs = jmpsOrNops.map(idxToFlip => for {
        flipped <- ZIO.fromTry(flip(idxToFlip)).orDie
        outcome <- determineOutcome(ProgramState(0, flipped, 0))
        _ <- console.putStrLn(s"Determined outcome of flipping $idxToFlip to $outcome")
        acc <- outcome match {
          case InfiniteLoop(_) => ZIO.fail(new Throwable("Infinite loop!"))
          case FinishAfterLastOp(acc) => ZIO.succeed(acc)
          case InvalidPC(_) => ZIO.fail(new Throwable("Invalid final pc!"))
        }
      } yield acc).toList
      acc <- ZIO.raceAll(programs.head, programs.tail)
    } yield acc
  }

  // TODO part2 alternative solution building a directed graph and doing some kind of smart graph search to find the initial state.

  val inputs = Map(
    "example" -> InputString(
      """nop +0
        |acc +1
        |jmp +4
        |acc +3
        |jmp -3
        |acc -99
        |acc +1
        |jmp -4
        |acc +6""".stripMargin),
    "puzzle" -> ResourceInput("day8puzzle.txt")
  )
}
