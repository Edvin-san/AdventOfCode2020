import zio.{RIO, ZEnv, ZIO, console}

trait Day[P1, P2] extends zio.App {
  def logic: RIO[ZEnv, Unit] = {
//    console.putStrLn(s"Going to run tests for $inputs") *>
    ZIO.foreach_(inputs) { case (label, input) =>
    for {
      _ <- console.putStrLn(s"----- $label -----")
      (p1, p2) <- part1(input) <&> part2(input)
      _ <- console.putStrLn(s"Part1 $p1")
      _ <- console.putStrLn(s"Part2 $p2\n")
    } yield ()
  }
  }

  def part1(in: String): RIO[ZEnv, P1]
  def part2(in: String): RIO[ZEnv, P2]

  val inputs: Map[String, String]

  def run(args: List[String]) =
    logic.exitCode
}
