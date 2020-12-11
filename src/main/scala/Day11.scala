import zio._
import Util.Vector._

object Day11 extends Day[Long, Long] {

  sealed trait PosState {
    def flip: PosState = this match {
      case Occupied => Empty
      case Empty => Occupied
    }
  }
  final case object Occupied extends PosState
  final case object Empty extends PosState

  def iterateUntilStabilized(initial: Map[Pos, PosState]): Map[Pos, PosState] = {
    val next = (for {
      (p, state) <- initial
      adjacentOcc = Dir.all8Dirs.map(p + _).flatMap(pos => initial.get(pos)).count(_ == Occupied)
      newState = state match {
        case Occupied if adjacentOcc >= 4 => state.flip
        case Empty if adjacentOcc == 0 => state.flip
        case _ => state
      }
    } yield p -> newState).toMap
    if (next == initial) next else iterateUntilStabilized(next)
  }

  def iterateUntilStabilized2(initial: Map[Pos, PosState]): Map[Pos, PosState] = {
    val next = (for {
      (p, state) <- initial
      adjacentOcc = Dir.all8Dirs.map(p + _).flatMap(pos => initial.get(pos)).count(_ == Occupied)
      newState = state match {
        case Occupied if adjacentOcc >= 4 => state.flip
        case Empty if adjacentOcc == 0 => state.flip
        case _ => state
      }
    } yield p -> newState).toMap
    if (next == initial) next else iterateUntilStabilized(next)
  }

  def part1(in: String) = Task.effect {
    val lines = in.split("\n")
    val m: Map[Pos, PosState] = (for {
      r <- 0 until lines.size
      c <- 0 until lines(r).size if lines(r)(c) != '.'
      state = lines(r)(c) match {
        case 'L' => Empty
        case '#' => Occupied
      }
    } yield Pos(c, r) -> state).toMap

    iterateUntilStabilized(m).map(_._2).count(_ == Occupied)
  }

  def part2(in: String) = Task.effect {
    ???
  }

  val inputs = Map(
    "example" -> InputString(
      """L.LL.LL.LL
        |LLLLLLL.LL
        |L.L.L..L..
        |LLLL.LL.LL
        |L.LL.LL.LL
        |L.LLLLL.LL
        |..L.L.....
        |LLLLLLLLLL
        |L.LLLLLL.L
        |L.LLLLL.LL""".stripMargin),
    "puzzle" -> ResourceInput("day11puzzle.txt")
  )
}
