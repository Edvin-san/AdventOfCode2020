import zio._

object Day17 extends Day[Long, Long] {
  case class Point3d(x: Int, y: Int, z: Int) {
    def neighbors: List[Point3d] =
      (for {
        xN <- (x - 1) to (x + 1)
        yN <- (y - 1) to (y + 1)
        zN <- (z - 1) to (z + 1) if !(xN == x && yN == y && zN == z)
      } yield Point3d(xN, yN, zN)).toList
  }

  def advance(active: Set[Point3d]): Set[Point3d] = {
    val allRelevantPoints = active ++ active.flatMap(_.neighbors)
    allRelevantPoints.map(p => (p, p.neighbors.filter(active(_)).size)).flatMap {
      case (p, aliveNeighbors) if active(p) && List(2, 3).contains(aliveNeighbors) => Some(p)
      case (p, aliveNeighbors) if !active(p) && aliveNeighbors == 3 => Some(p)
      case _ => None
    }
  }

  def parseInitial(s: String): Set[Point3d] =
    (for {
      (line, row) <- s.split("\n").zipWithIndex
      (c, col) <- line.zipWithIndex
      if c == '#'
    } yield Point3d(col, row, 0)).toSet

  def part1(in: String) = Task.effect {
    val initial = parseInitial(in)
    val sequence = LazyList.iterate(initial)(advance)
    sequence(6).size
  }

  case class Point4d(x: Int, y: Int, z: Int, w: Int) {
    def neighbors: List[Point4d] =
      (for {
        xN <- (x - 1) to (x + 1)
        yN <- (y - 1) to (y + 1)
        zN <- (z - 1) to (z + 1)
        wN <- (w - 1) to (w + 1) if !(xN == x && yN == y && zN == z && wN == w)
      } yield Point4d(xN, yN, zN, wN)).toList
  }

  def advance4d(active: Set[Point4d]): Set[Point4d] = {
    val allRelevantPoints = active ++ active.flatMap(_.neighbors)
    allRelevantPoints.map(p => (p, p.neighbors.filter(active(_)).size)).flatMap {
      case (p, aliveNeighbors) if active(p) && List(2, 3).contains(aliveNeighbors) => Some(p)
      case (p, aliveNeighbors) if !active(p) && aliveNeighbors == 3 => Some(p)
      case _ => None
    }
  }

  def parseInitial4d(s: String): Set[Point4d] =
    (for {
      (line, row) <- s.split("\n").zipWithIndex
      (c, col) <- line.zipWithIndex
      if c == '#'
    } yield Point4d(col, row, 0, 0)).toSet

  def part2(in: String) = Task.effect {
    val initial = parseInitial4d(in)
    val sequence = LazyList.iterate(initial)(advance4d)
    sequence(6).size
  }

  val inputs = Map(
    "example" -> InputString(
      """.#.
        |..#
        |###""".stripMargin),
    "puzzle" -> InputString(
      """..##.#.#
        |##....#.
        |....####
        |#..##...
        |#..#.##.
        |.#..#...
        |##...##.
        |.#...#..
        |""".stripMargin)
  )
}
