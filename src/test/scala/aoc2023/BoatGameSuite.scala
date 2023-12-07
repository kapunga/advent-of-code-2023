package aoc2023

import cats.effect.IO
import munit.CatsEffectSuite

class BoatGameSuite extends CatsEffectSuite {
  val partOneExample: List[String] = List(
    "Time:      7  15   30",
    "Distance:  9  40  200")

  val partOneSolution: String = "288"

  val partTwoSolution: String = "71503"

  test("Part one solution works against example.") {
    BoatGame.optionsMulti(fs2.Stream.emits(partOneExample))
      .flatMap(result => IO(assertEquals(result, partOneSolution)))
  }

  test("Part two solution works against example.") {
    BoatGame.optionsSingle(fs2.Stream.emits(partOneExample))
      .flatMap(result => IO(assertEquals(result, partTwoSolution)))
  }
}
