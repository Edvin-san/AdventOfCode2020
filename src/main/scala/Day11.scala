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

  def prettyPrintMap(m: Map[Pos, PosState]): String = {
    val mC = m.map { case (p, state) => state match {
      case Occupied => p -> '#'
      case Empty => p -> 'L'
    }}
    (for {
      y <- 0 to m.keys.map(_.y).max
      line = (for {
        x <- 0 to m.keys.map(_.x).max
      } yield mC.getOrElse(Pos(x, y), '.')).mkString
    } yield line).mkString("\n")
  }

  def iterateUntilStabilized2(initial: Map[Pos, PosState]): Map[Pos, PosState] = {
    println(prettyPrintMap(initial))
    val next = (for {
      (p, state) <- initial
      firstInAllDirs: Map[Double, Pos] = initial.keySet.excl(p).map(p2 => (p2, PolarCoordinate.make(p, p2))).groupBy(_._2.theta).map {
        case (theta, posPolar) => (theta, posPolar.toList.sortBy(_._2.r).map(_._1).head)
      }
      _ = println(s"For $p")
      _ = println(firstInAllDirs)
      seenOccupied = Dir.all8Dirs.flatMap { dir =>
        val theta = PolarCoordinate.make(p, p + dir).theta
        firstInAllDirs.get(theta)
      }.count(_ == Occupied)
      newState = state match {
        case Occupied if seenOccupied >= 5 => state.flip
        case Empty if seenOccupied == 0 => state.flip
        case _ => state
      }
    } yield p -> newState).toMap
    if (next == initial) next else iterateUntilStabilized2(next)
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
    val lines = in.split("\n")
    val m: Map[Pos, PosState] = (for {
      r <- 0 until lines.size
      c <- 0 until lines(r).size if lines(r)(c) != '.'
      state = lines(r)(c) match {
        case 'L' => Empty
        case '#' => Occupied
      }
    } yield Pos(c, r) -> state).toMap

    iterateUntilStabilized2(m).map(_._2).count(_ == Occupied)
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
