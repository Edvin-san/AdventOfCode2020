import Util.Vector.Pos
import zio._

object Day20 extends Day[BigInt, BigInt] {
  sealed trait Direction {
    def opposite: Direction = this match {
      case North => South
      case East => West
      case South => North
      case West => East
    }
  }
  object Direction {
    def values: List[Direction] = List(North, East, South, West)
  }
  case object North extends Direction
  case object East extends Direction
  case object South extends Direction
  case object West extends Direction

  case class TileOrientation(matrix: List[String]) {
    lazy val height = matrix.size
    lazy val width = matrix.head.size
    lazy val borderHash: Map[Direction, Int] = borders.view.mapValues(_.hashCode).toMap
    lazy val borders: Map[Direction, String] = Map(
      North -> matrix.head,
      East -> matrix.map(_.last).mkString,
      South -> matrix.last,
      West -> matrix.map(_.head).mkString
    )
    lazy val prettyPrint: String = matrix.mkString("\n")
  }

  object TileOrientation {
    def flip(t: TileOrientation): TileOrientation = TileOrientation(t.matrix.map(_.reverse))

    def prettyPrintMany(tos: List[List[TileOrientation]]): String = tos.map(x => (0 until x.head.matrix.size).map(row => x.map(_.matrix(row)).mkString(" ")).mkString("\n")).mkString("\n\n")
    def prettyPrintManyNoGaps(tos: List[List[TileOrientation]]): String = tos.map(x => (0 until x.head.matrix.size).map(row => x.map(_.matrix(row)).mkString).mkString("\n")).mkString("\n")
    def removeBorders(t: TileOrientation): TileOrientation = TileOrientation(t.matrix.drop(1).dropRight(1).map(_.drop(1).dropRight(1)))
  }

  type TileId = Int
  type OrientationIdx = Int

  case class Tile(id: TileId, orientations: List[TileOrientation]) {
    def prettyPrint(i: OrientationIdx): String = s"Tile $id:\n${orientations(i).prettyPrint}"
  }

  object Tile {
    // Clockwise
    private def rotateMatrix(matrix: List[String]): List[String] = (for {
      col <- 0 until matrix.head.size
    } yield matrix.map(_(col)).reverse.mkString).toList

    def parse(s: String): Tile = s.split("\n").toList match {
      case firstLine :: lines =>
        val id = firstLine.drop(5).dropRight(1).toInt
        val rotated = LazyList.iterate(lines)(rotateMatrix).take(4).map(TileOrientation(_)).toList
        val orientations = rotated ++ rotated.map(TileOrientation.flip(_))
        Tile(id, orientations)
    }
  }

  def parseInput(in: String): List[Tile] = in.split("\n\n").map(Tile.parse).toList

  case class TileOrientationId(tileId: TileId, orientationId: OrientationIdx)
  type TileConnectionMap = Map[TileId, Map[OrientationIdx, Map[Direction, Set[TileOrientationId]]]]

  def findConnections(tiles: Map[TileId, Tile]): TileConnectionMap =
    (for {
      tileId <- tiles.keys
      orientationMap = (for {
        orientationIdx <- 0 until tiles(tileId).orientations.size
        directionMap = (for {
          dir <- Direction.values
          dirHash = tiles(tileId).orientations(orientationIdx).borderHash(dir)
          matchingTileOrientations = tiles.removed(tileId).toList.flatMap {
            case (tileId, tile) => tile.orientations.zipWithIndex.filter(t => t._1.borderHash(dir.opposite) == dirHash).map(t => TileOrientationId(tileId, t._2))
          }.toSet
        } yield dir -> matchingTileOrientations).toMap
      } yield orientationIdx -> directionMap).toMap
    } yield tileId -> orientationMap).toMap

  def solve1(tiles: Map[TileId, Tile], tileConn: TileConnectionMap): Option[List[List[TileOrientationId]]] = {
    val gridLen = math.sqrt(tiles.keys.size).toInt

    def updateConstraints(
         newAssignment: (Pos, TileOrientationId),
         assignments: Map[Pos, TileOrientationId],
         constraints: Map[Pos, Set[TileOrientationId]]): Option[Map[Pos, Set[TileOrientationId]]] = {
      val (pos, tileOrientationId) = newAssignment
      if (assignments.isDefinedAt(pos) || !constraints(pos).contains(tileOrientationId)) None
      else {
        val newConstraints = constraints.view.mapValues(s => s.filter(_.tileId != tileOrientationId.tileId)).toMap ++ Map(pos -> Set(tileOrientationId))
        if (newConstraints.values.exists(_.size == 0)) None
        else {
          val rightPos = pos.copy(x = pos.x + 1)
          val rightConstraints = constraints.get(rightPos)
          val downPos = pos.copy(y = pos.y + 1)
          val downConstraints = constraints.get(downPos)
          val rightConn = tileConn(tileOrientationId.tileId)(tileOrientationId.orientationId)(East)
          val rightRefined = rightConstraints.map(_.intersect(rightConn))
          val downConn = tileConn(tileOrientationId.tileId)(tileOrientationId.orientationId)(South)
          val downRefined = downConstraints.map(_.intersect(downConn))
          val updatedConstraints = newConstraints ++ rightRefined.map(s => Map(rightPos -> s)).getOrElse(Map()) ++ downRefined.map(s => Map(downPos -> s)).getOrElse(Map())
          if (updatedConstraints.values.exists(_.size == 0)) None
          else Some(updatedConstraints)
        }
      }
    }

    def nextPos(pos: Pos): Option[Pos] =
      if (pos == Pos(gridLen - 1, gridLen - 1)) None
      else if (pos.x >= gridLen - 1) Some(Pos(0, pos.y + 1))
      else Some(Pos(pos.x + 1, pos.y))

    def inner(curPos: Pos, assignments: Map[Pos, TileOrientationId], constraints: Map[Pos, Set[TileOrientationId]], indent: Int = 0): Option[Map[Pos, TileOrientationId]] = {
      val possibleOrientations = constraints(curPos)
      val next = nextPos(curPos)
      val ind = " ".repeat(indent)
      LazyList.from(possibleOrientations).map {
        tileOrientationId =>
          val updatedConstraints = updateConstraints(curPos -> tileOrientationId, assignments, constraints)
          updatedConstraints.flatMap { newConstraints =>
            val updatedAssignments = assignments.updated(curPos, tileOrientationId)
            next match {
              case Some(pos) => inner(pos, updatedAssignments, newConstraints, indent + 1)
              case None => Some(updatedAssignments)
            }
          }
      }.find(_.isDefined).flatten
    }

    val positions = for {
      y <- 0 until gridLen
      x <- 0 until gridLen
    } yield Pos(x, y)
    val allTileOrientations = tiles.values.flatMap(t => t.orientations.zipWithIndex.map(_._2).map(o => TileOrientationId(t.id, o))).toSet
    val initialConstraints = positions.map(p => p -> allTileOrientations).toMap

    val solution = inner(Pos(0, 0), Map(), initialConstraints)
    solution.map { posMap =>
      posMap.groupBy(_._1.y).map {
        case (y, xMap) => y -> xMap.toList.sortBy(_._1.x).map(_._2)
      }.toList.sortBy(_._1).map(_._2)
    }
  }

  def part1(in: String) = Task.effect {
    val tiles = parseInput(in).map(t => (t.id, t)).toMap
    val tileConnections = findConnections(tiles)
    val sol = solve1(tiles, tileConnections).get
//    println(sol.map(_.map(_.tileId).mkString(" ")).mkString("\n"))
    val corners = List(sol.head.head, sol.head.last, sol.last.head, sol.last.last).map(_.tileId)
    corners.map(BigInt(_)).product
  }

  def findMonsters(tile: TileOrientation): Int = {
    val p1 = raw"..................#.".r
    val p2 = raw"#....##....##....###".r
    val p3 = raw".#..#..#..#..#..#...".r
    val rSize = p1.regex.size
    val matrix = tile.matrix
    val matches = for {
      r <- 0 until (matrix.size - 3)
      c <- 0 until (matrix.head.size - rSize)
      if p1.matches(matrix(r).drop(c).take(rSize)) && p2.matches(matrix(r+1).drop(c).take(rSize)) && p3.matches(matrix(r + 2).drop(c).take(rSize))
    } yield (r, c)
    matches.size
  }

  def part2(in: String) = Task.effect {
    val tiles = parseInput(in).map(t => (t.id, t)).toMap
    val tileConnections = findConnections(tiles)
    val sol = solve1(tiles, tileConnections).get
    val tilesAligned = sol.map(_.map {
      case TileOrientationId(tileId, orientationIdx) => tiles(tileId).orientations(orientationIdx)
    })
    val withoutBorders = tilesAligned.map(_.map(TileOrientation.removeBorders(_)))
    val bigTile = Tile.parse("Tile 0:\n" + TileOrientation.prettyPrintManyNoGaps(withoutBorders))
    val numHashtag = bigTile.orientations.head.matrix.flatten.count(_ == '#')
    val (finalTile, numMonsters) = bigTile.orientations.map(t => (t, findMonsters(t))).maxBy(_._2)
    numHashtag - numMonsters*15
//    println(TileOrientation.prettyPrintMany(List(bigTile.orientations.take(4), bigTile.orientations.drop(4))))
  }

  val inputs = Map(
    "example" -> InputString(
      """Tile 2311:
        |..##.#..#.
        |##..#.....
        |#...##..#.
        |####.#...#
        |##.##.###.
        |##...#.###
        |.#.#.#..##
        |..#....#..
        |###...#.#.
        |..###..###
        |
        |Tile 1951:
        |#.##...##.
        |#.####...#
        |.....#..##
        |#...######
        |.##.#....#
        |.###.#####
        |###.##.##.
        |.###....#.
        |..#.#..#.#
        |#...##.#..
        |
        |Tile 1171:
        |####...##.
        |#..##.#..#
        |##.#..#.#.
        |.###.####.
        |..###.####
        |.##....##.
        |.#...####.
        |#.##.####.
        |####..#...
        |.....##...
        |
        |Tile 1427:
        |###.##.#..
        |.#..#.##..
        |.#.##.#..#
        |#.#.#.##.#
        |....#...##
        |...##..##.
        |...#.#####
        |.#.####.#.
        |..#..###.#
        |..##.#..#.
        |
        |Tile 1489:
        |##.#.#....
        |..##...#..
        |.##..##...
        |..#...#...
        |#####...#.
        |#..#.#.#.#
        |...#.#.#..
        |##.#...##.
        |..##.##.##
        |###.##.#..
        |
        |Tile 2473:
        |#....####.
        |#..#.##...
        |#.##..#...
        |######.#.#
        |.#...#.#.#
        |.#########
        |.###.#..#.
        |########.#
        |##...##.#.
        |..###.#.#.
        |
        |Tile 2971:
        |..#.#....#
        |#...###...
        |#.#.###...
        |##.##..#..
        |.#####..##
        |.#..####.#
        |#..#.#..#.
        |..####.###
        |..#.#.###.
        |...#.#.#.#
        |
        |Tile 2729:
        |...#.#.#.#
        |####.#....
        |..#.#.....
        |....#..#.#
        |.##..##.#.
        |.#.####...
        |####.#.#..
        |##.####...
        |##..#.##..
        |#.##...##.
        |
        |Tile 3079:
        |#.#.#####.
        |.#..######
        |..#.......
        |######....
        |####.#..#.
        |.#...#.##.
        |#.#####.##
        |..#.###...
        |..#.......
        |..#.###...""".stripMargin),
    "puzzle" -> ResourceInput("day20puzzle.txt")
  )
}
