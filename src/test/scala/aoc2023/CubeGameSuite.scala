package aoc2023

import cats.effect.IO
import munit.CatsEffectSuite

class CubeGameSuite extends CatsEffectSuite {
  val partOneExample: List[String] = List(
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

  val partOneSampleDraw: Draw = Draw(red = 12, green = 13, blue = 14)

  val partOneSolution: String = "8"

  val partTwoSolution: String = "2286"

  test("Part one solution works against example.") {
    CubeGame.solve(partOneSampleDraw)(fs2.Stream.emits(partOneExample))
      .flatMap(result => IO(assertEquals(result, partOneSolution)))
  }

  test("Part two solution works against example.") {
    CubeGame.power(fs2.Stream.emits(partOneExample))
      .flatMap(result => IO(assertEquals(result, partTwoSolution)))
  }
}
