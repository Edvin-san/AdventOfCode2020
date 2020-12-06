import java.io.{File, IOException}

import zio.{RIO, ZEnv, ZIO, ZManaged, console}
import Util.normalizeNewLine
import zio.blocking.Blocking

trait Day[P1, P2] extends zio.App {
  def logic: RIO[ZEnv, Unit] = {
//    console.putStrLn(s"Going to run tests for $inputs") *>
    ZIO.foreach_(inputs) { case (label, input) =>
    for {
      _ <- console.putStrLn(s"----- $label -----")
      _ <- getInput(input).map(normalizeNewLine).use { in =>
        for {
          (p1, p2) <- part1(in).map(_.toString).catchSome {
            case _: NotImplementedError => ZIO.succeed("Not implemented.")
          } <*> part2(in).map(_.toString).catchSome {
            case _: NotImplementedError => ZIO.succeed("Not implemented.")
          }
          _ <- console.putStrLn(s"Part1 $p1")
          _ <- console.putStrLn(s"Part2 $p2\n")
        } yield ()
      }
    } yield ()
  }
  }

  def part1(in: String): RIO[ZEnv, P1]
  def part2(in: String): RIO[ZEnv, P2]

  sealed trait Input
  case class InputString(value: String) extends Input
  case class ResourceInput(value: String) extends Input

  def getInput(in: Input): ZManaged[Blocking, IOException, String] = in match {
    case InputString(value) => ZManaged.succeed(value)
    case ResourceInput(path) => {
      val resourcePath = new File(getClass.getClassLoader.getResource(path).getPath).getPath
      ZManaged.readFile(resourcePath).mapM(_.readAll(4096).map(_.map(_.toChar).mkString).mapError {
        case Some(iOException) => iOException
        case None => new IOException("readAll failed with None")
      })
    }
  }

  def inputs: Map[String, Input]

  def run(args: List[String]) =
    logic.exitCode
}
