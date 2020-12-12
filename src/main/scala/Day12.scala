import Util.Vector.{Dir, PolarCoordinate, Pos}
import zio._
import zio.console.Console
import zio.duration.durationInt

object Day12 extends Day[Long, Long] {

  def p1Polar(in: String): ZIO[Console, Nothing, List[(String, Pos, Dir)]] = for {
    facing <- Ref.make(Dir.East)
    pos <- Ref.make(Pos(0, 0))
    history <- Ref.make(List(("Start", Pos(0, 0), Dir.East)))
    move = (d: Dir, i: Int) => pos.update(_ + d * i)
    turnCCW = (degrees: Int) => ZIO.foreach_(1 to degrees / 90)(_ => facing.update(_.rotated90CCW))
    turnCW = (degrees: Int) => ZIO.foreach_(1 to degrees / 90)(_ => facing.update(_.rotated90CW))
    _ <- ZIO.foreach_(in.split("\n")) { line =>
      for {
        _ <- (line.head, line.tail.toInt) match {
          case 'N' -> i => move(Dir.North, i)
          case 'S' -> i => move(Dir.South, i)
          case 'E' -> i => move(Dir.East, i)
          case 'W' -> i => move(Dir.West, i)
          case 'L' -> degrees => turnCCW(degrees)
          case 'R' -> degrees => turnCW(degrees)
          case 'F' -> i => facing.get.flatMap(d => move(d, i))
        }
        p <- pos.get
        d <- facing.get
        _ <- history.update(_.appended((line, p, d)))
      } yield ()
    }
    h <- history.get
  } yield h

  def p2Polar(in: String): ZIO[Console, Nothing, List[(String, Pos, Dir)]] = for {
    ship <- Ref.make(Pos(0, 0))
    waypoint <- Ref.make(Dir(10, 1))
    history <- Ref.make(List(("Start", Pos(0, 0), Dir(10, 1))))
    move = (d: Dir, i: Int) => ship.update(_ + d * i)
    moveWaypoint = (d: Dir, i: Int) => waypoint.update(_ + (d * i))
    turnCCW = (degrees: Int) => ZIO.foreach_(1 to degrees / 90)(_ => waypoint.update(_.rotated90CCW))
    turnCW = (degrees: Int) => ZIO.foreach_(1 to degrees / 90)(_ => waypoint.update(_.rotated90CW))
    _ <- ZIO.foreach_(in.split("\n")) { line =>
      for {
        _ <- (line.head, line.tail.toInt) match {
          case 'N' -> i => moveWaypoint(Dir.North, i)
          case 'S' -> i => moveWaypoint(Dir.South, i)
          case 'E' -> i => moveWaypoint(Dir.East, i)
          case 'W' -> i => moveWaypoint(Dir.West, i)
          case 'L' -> degrees => turnCCW(degrees)
          case 'R' -> degrees => turnCW(degrees)
          case 'F' -> i => waypoint.get.flatMap(d => move(d, i))
        }
        p <- ship.get
        d <- waypoint.get
        _ <- history.update(_.appended((line, p, d)))
      } yield ()
    }
    h <- history.get
  } yield h

  def line(from: Pos, to: Pos): List[Pos] = {
    val p = to - from
    val g = Util.gcd(p.x, p.y)
    val d = p.copy(x = p.x/g, y = p.y/g).toDir
    LazyList.iterate(from)(_ + d).takeWhile(_ != to).appended(to).toList
  }

  def visualize(path: List[(String, Pos, Dir)]) = {
    val positions = path.map(_._2)
//    val minX = positions.map(_.x).min
//    val maxX = positions.map(_.x).max
//    val minY = positions.map(_.y).min
//    val maxY = positions.map(_.y).max
    val uniquePositions = positions.head :: positions.sliding(2).collect { case Seq(a, b) if a != b => b }.toList
    ZIO.foreach_(2 to uniquePositions.size) { idx =>
      val subpath = uniquePositions.take(idx)
      val (prevs, last) = (subpath.dropRight(1), subpath.last)
      val prevPathPoints = prevs.sliding(2, 1).flatMap {
        case List(p1, p2) => line(p1, p2)
        case _ => Nil
      }.toSet
      val lastPathPoints = line(prevs.lastOption.getOrElse(Pos(0, 0)), last)
      ZIO.foreach_(1 to lastPathPoints.size) { j =>
        val lastSubpath = lastPathPoints.take(j)
        val (lastPrevs, veryLast) = (lastSubpath.dropRight(1), lastSubpath.last)
        val allPrevs = prevPathPoints ++ lastPrevs.toSet
        val mapString = (for {
          y <- (veryLast.y + 15).to(veryLast.y - 10, -1)
          line = (for {
            x <- (veryLast.x - 50) to (veryLast.x + 150)
            p = Pos(x, y)
            c = if (p == veryLast) 'S'
            else if (allPrevs.contains(p)) '.'
            else ' '
          } yield c).mkString("")
        } yield line).mkString("\n")
        console.putStrLn(mapString + "\n\n")
      }
    }
  }

  def part1(in: String) = for {
    p <- p1Polar(in)
//    minX = p.map(_._2.x).min
//    maxX = p.map(_._2.x).max
//    minY = p.map(_._2.y).min
//    maxY = p.map(_._2.y).max
//    _ <- console.putStrLn(s"$minX $maxX, $minY $maxY")
//    p2 = p.map { case (str, pos, dir) => (str, pos.copy(x = (pos.x - minX)*100/(maxX - minX), y = (pos.y - minY)*25/(maxY - minY)), dir) }
//    _ <- visualize(p2)
    last = p.last._2
  } yield last.x.abs + last.y.abs

  def part2(in: String) = for {
    p <- p2Polar(in)
//    minX = p.map(_._2.x).min
//    maxX = p.map(_._2.x).max
//    minY = p.map(_._2.y).min
//    maxY = p.map(_._2.y).max
//    _ <- console.putStrLn(s"$minX $maxX, $minY $maxY")
//    p2 = p.map { case (str, pos, dir) => (str, pos.copy(x = (pos.x - minX)*100/(maxX - minX), y = (pos.y - minY)*25/(maxY - minY)), dir) }
//    _ <- visualize(p2)
    last = p.last._2
  } yield last.x.abs + last.y.abs

  val inputs = Map(
    "example" -> InputString(
      """F10
        |N3
        |F7
        |R90
        |F11""".stripMargin),
    "puzzle" -> ResourceInput("day12puzzle.txt")
  )
}
