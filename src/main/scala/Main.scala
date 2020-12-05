import zio.ExitCode
import zio.console._

object Main extends zio.App {

  def run(args: List[String]) =
    logic.exitCode

  val logic = for {
    _ <- putStrLn(Day4.part2(Day4.puzzle).toString)
  } yield ExitCode.success
}
