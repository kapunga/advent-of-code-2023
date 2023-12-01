package aoc2023

import cats.effect.IO
import munit.CatsEffectSuite

class CalibrationConfigSuite extends CatsEffectSuite {
  val partOneExample: List[String] = List(
    "1abc2",
    "pqr3stu8vwx",
    "a1b2c3d4e5f",
    "treb7uchet")

  val partOneSolution: String = "142"

  val partTwoExample: List[String] = List(
    "two1nine",
    "eightwothree",
    "abcone2threexyz",
    "xtwone3four",
    "4nineeightseven2",
    "zoneight234",
    "7pqrstsixteen")

  val partTwoSolution: String = "281"

  test("Part one solution works against example.") {
    CalibrationConfig.read(false)(fs2.Stream.emits(partOneExample))
      .flatMap(result => IO(assertEquals(result, partOneSolution)))
  }

  test("Part two solution works against example.") {
    CalibrationConfig.read(true)(fs2.Stream.emits(partTwoExample))
      .flatMap(result => IO(assertEquals(result, partTwoSolution)))
  }
}
