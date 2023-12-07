package aoc2023

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import fs2.text

object Aoc2023 extends IOApp {
  case class PuzzleDef(file: String, solver: fs2.Stream[IO, String] => IO[String])

  val puzzles: Map[String, PuzzleDef] = Map(
    "d01a" -> PuzzleDef("aoc2023/d01.txt", CalibrationConfig.read(false)),
    "d01b" -> PuzzleDef("aoc2023/d01.txt", CalibrationConfig.read(true)),
    "d02a" -> PuzzleDef("aoc2023/d02.txt", CubeGame.solve(CubeGame.partOneDraw)),
    "d02b" -> PuzzleDef("aoc2023/d02.txt", CubeGame.power),
    "d03a" -> PuzzleDef("aoc2023/d03.txt", EngineSchematic.partNumbers),
    "d03b" -> PuzzleDef("aoc2023/d03.txt", EngineSchematic.gearRatios),
    "d04a" -> PuzzleDef("aoc2023/d04.txt", Lottery.points),
    "d04b" -> PuzzleDef("aoc2023/d04.txt", Lottery.totalTickets),
    "d05a" -> PuzzleDef("aoc2023/d05.txt", Catalogue.lookup),
    "d05b" -> PuzzleDef("aoc2023/d05.txt", Catalogue.lookupRange),
    "d06a" -> PuzzleDef("aoc2023/d06.txt", BoatGame.optionsMulti),
    "d06b" -> PuzzleDef("aoc2023/d06.txt", BoatGame.optionsSingle))

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
      .timed
      .flatMap((t, _) => Console[IO].println(s"Timing: ${t.toMillis} ms"))
      .as(ExitCode.Success)
  }
}
