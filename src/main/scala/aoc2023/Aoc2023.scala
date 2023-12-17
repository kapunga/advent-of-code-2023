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
    "d06b" -> PuzzleDef("aoc2023/d06.txt", BoatGame.optionsSingle),
    "d07a" -> PuzzleDef("aoc2023/d07.txt", CamelCards.winnings),
    "d07b" -> PuzzleDef("aoc2023/d07.txt", CamelCards.wildWinnings),
    "d08a" -> PuzzleDef("aoc2023/d08.txt", CamelDirections.follow),
    "d08b" -> PuzzleDef("aoc2023/d08.txt", CamelDirections.followAll),
    "d09a" -> PuzzleDef("aoc2023/d09.txt", SandSensor.takeReading),
    "d09b" -> PuzzleDef("aoc2023/d09.txt", SandSensor.takeReadingInitial),
    "d10a" -> PuzzleDef("aoc2023/d10.txt", PipeMaze.maxLength),
    "d10b" -> PuzzleDef("aoc2023/d10.txt", PipeMaze.interior),
    "d11a" -> PuzzleDef("aoc2023/d11.txt", GalaxyMap.distances(1, _)),
    "d11b" -> PuzzleDef("aoc2023/d11.txt", GalaxyMap.distances(1000000, _)),
    "d12a" -> PuzzleDef("aoc2023/d12.txt", HotSpringDiagrams.possibilities),
    "d12b" -> PuzzleDef("aoc2023/d12.txt", HotSpringDiagrams.bigPossibilities),
    "d13a" -> PuzzleDef("aoc2023/d13.txt", MirrorFinder.findAll),
    "d13b" -> PuzzleDef("aoc2023/d13.txt", MirrorFinder.findSmudged),
    "d14a" -> PuzzleDef("aoc2023/d14.txt", RockPlatform.checkLoad),
    "d14b" -> PuzzleDef("aoc2023/d14.txt", RockPlatform.loadTest),
    "d15a" -> PuzzleDef("aoc2023/d15.txt", AsciiHash.runHash),
    "d15b" -> PuzzleDef("aoc2023/d15.txt", AsciiHash.lensePower),
    "d16a" -> PuzzleDef("aoc2023/d16.txt", LavaProduction.covered),
    "d16b" -> PuzzleDef("aoc2023/d16.txt", LavaProduction.maxCovered))

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
