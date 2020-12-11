import zio._
import Util.Vector._

import scala.annotation.tailrec

object Day11 extends Day[Long, Long] {

  sealed trait PosState {
    def flip: PosState = this match {
      case Occupied => Empty
      case Empty => Occupied
    }
  }

  final case object Occupied extends PosState

  final case object Empty extends PosState

  @tailrec
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
    }
    }
    (for {
      y <- 0 to m.keys.map(_.y).max
      line = (for {
        x <- 0 to m.keys.map(_.x).max
      } yield mC.getOrElse(Pos(x, y), '.')).mkString
    } yield line).mkString("\n")
  }

  @tailrec
  def iterateUntilStabilized2(initial: Map[Pos, PosState], adj: Map[Pos, List[Pos]]): Map[Pos, PosState] = {
    val next = (for {
      (p, state) <- initial
      seenOccupied = adj(p).count(x => initial(x) == Occupied)
      newState = state match {
        case Occupied if seenOccupied >= 5 => state.flip
        case Empty if seenOccupied == 0 => state.flip
        case _ => state
      }
    } yield p -> newState).toMap
    if (next == initial) next else iterateUntilStabilized2(next, adj)
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

  def findNeighbors(p: Pos, all: Set[Pos], maxX: Int, maxY: Int): List[Pos] =
    Dir.all8Dirs.flatMap { dir =>
      val x = LazyList.iterate(p + dir)(_ + dir).dropWhile(n => n.x <= maxX && n.x >= 0 && n.y >= 0 && n.y <= maxY && !all.contains(n)).head
      Option.when(all.contains(x))(x)
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

    val maxX = m.keys.map(_.x).max
    val maxY = m.keys.map(_.y).max
    // Only need to compute the neighbours once, will always be the same.
    val adj = m.map {
      case p -> _ => p -> findNeighbors(p, m.keySet, maxX, maxY)
    }

    // This computation was slooow, O((x*y)^2) at least. Puzzle optimized from 81s to 0.6s.
    //    val adj = (for {
    //      (p, _) <- m
    //      firstInAllDirs: Map[Double, Pos] = m.keySet.excl(p).map(p2 => (p2, PolarCoordinate.make(p, p2))).groupBy(_._2.theta).map {
    //        case (theta, posPolar) => (theta, posPolar.toList.sortBy(_._2.r).map(_._1).head)
    //      }
    //      seen = Dir.all8Dirs.flatMap { dir =>
    //        val theta = PolarCoordinate.make(p, p + dir).theta
    //        firstInAllDirs.get(theta)
    //      }
    //      _ = println(s"Adj $p done")
    //    } yield p -> seen).toMap

    iterateUntilStabilized2(m, adj).map(_._2).count(_ == Occupied)
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
