import Util.Vector.{Dir, Pos}
import zio._

import scala.annotation.tailrec

object Day24 extends Day[Long, Long] {
  val all6dirs = List(
    Dir(1, 0),
    Dir(0, 1),
    Dir(-1, 1),
    Dir(-1, 0),
    Dir(0, -1),
    Dir(1, -1),
  )

  def neighbors(p: Pos): List[Pos] = all6dirs.map(d => p + d)

  def hexDirToDir(hexDir: String): Dir = hexDir match {
    case "e" => Dir(1, 0)
    case "ne" => Dir(0, 1)
    case "nw" => Dir(-1, 1)
    case "w" => Dir(-1, 0)
    case "sw" => Dir(0, -1)
    case "se" => Dir(1, -1)
  }

  @tailrec
  def tilePathToPos(tilePath: List[String], origin: Pos = Pos(0, 0)): Pos = tilePath match {
    case head :: tail => tilePathToPos(tail, origin + hexDirToDir(head))
    case Nil => origin
  }

  def updateBlackPositions(black: Set[Pos]): Set[Pos] = {
    val allRelevantPos = black.flatMap(p => p :: neighbors(p))
    def numBlackNeighbors(p: Pos): Int = neighbors(p).filter(black(_)).size
    allRelevantPos.filter { p =>
      val bn = numBlackNeighbors(p)
      if (black(p)) bn == 1 || bn == 2 else bn == 2
    }
  }

  def part1(in: String) = Task.effect {
    val occurrences = in.split("\n").map { line =>
      val tilePath = line.replace("e", "e,").replace("w", "w,").split(",").toList
      tilePathToPos(tilePath)
    }.groupBy(identity).view.mapValues(_.size).toMap
    occurrences.filter(t => t._2 % 2 == 1).size
  }

  def part2(in: String) = Task.effect {
    val occurrences = in.split("\n").map { line =>
      val tilePath = line.replace("e", "e,").replace("w", "w,").split(",").toList
      tilePathToPos(tilePath)
    }.groupBy(identity).view.mapValues(_.size).toMap
    val blackPositions = occurrences.filter(t => t._2 % 2 == 1).keySet
    val sequence = LazyList.iterate(blackPositions)(updateBlackPositions)
    sequence(100).size
  }

  val inputs = Map(
    "example" -> InputString(
      """sesenwnenenewseeswwswswwnenewsewsw
        |neeenesenwnwwswnenewnwwsewnenwseswesw
        |seswneswswsenwwnwse
        |nwnwneseeswswnenewneswwnewseswneseene
        |swweswneswnenwsewnwneneseenw
        |eesenwseswswnenwswnwnwsewwnwsene
        |sewnenenenesenwsewnenwwwse
        |wenwwweseeeweswwwnwwe
        |wsweesenenewnwwnwsenewsenwwsesesenwne
        |neeswseenwwswnwswswnw
        |nenwswwsewswnenenewsenwsenwnesesenew
        |enewnwewneswsewnwswenweswnenwsenwsw
        |sweneswneswneneenwnewenewwneswswnese
        |swwesenesewenwneswnwwneseswwne
        |enesenwswwswneneswsenwnewswseenwsese
        |wnwnesenesenenwwnenwsewesewsesesew
        |nenewswnwewswnenesenwnesewesw
        |eneswnwswnwsenenwnwnwwseeswneewsenese
        |neswnwewnwnwseenwseesewsenwsweewe
        |wseweeenwnesenwwwswnew""".stripMargin),
    "puzzle" -> ResourceInput("day24puzzle.txt")
  )
}
