package aoc2023

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import fs2.text

object Aoc2023 extends IOApp {
  case class PuzzleDef(file: String, solver: fs2.Stream[IO, String] => IO[String])

  val puzzles: Map[String, PuzzleDef] = Map(
    "d01a" -> PuzzleDef("aoc2023/d01a.txt", CalibrationConfig.read(false)),
    "d01b" -> PuzzleDef("aoc2023/d01a.txt", CalibrationConfig.read(true)),
    "d02a" -> PuzzleDef("aoc2023/d02a.txt", CubeGame.solve(CubeGame.partOneDraw)),
    "d02b" -> PuzzleDef("aoc2023/d02a.txt", CubeGame.power)
  )

  override def run(args: List[String]): IO[ExitCode] = {
    args.headOption match
      case Some(puzzle) =>
        puzzles.get(puzzle).map(doPuzzle)
          .getOrElse(Console[IO].println(s"Unrecognized puzzle: '$puzzle'").as(ExitCode.Success))
      case None => Console[IO].println("You must input a puzzle name.").as(ExitCode.Success)
  }

  private def doPuzzle(pd: PuzzleDef): IO[ExitCode] = {
    val puzzleStream =
      fs2.io.readClassLoaderResource[IO](pd.file)
        .through(text.utf8.decode)
        .through(text.lines)

    pd.solver(puzzleStream)
      .flatMap(result => Console[IO].println(s"Puzzle Result: $result"))
      .as(ExitCode.Success)
  }
}
