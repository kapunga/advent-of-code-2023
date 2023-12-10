package aoc2023

import cats.effect.IO
import munit.CatsEffectSuite

class SandSensorSuite extends CatsEffectSuite {
  val partOneExample: List[String] = List(
    "0 3 6 9 12 15",
    "1 3 6 10 15 21",
    "10 13 16 21 30 45")

  val partOneSolution: String = "114"

  val partTwoSolution: String = "2"

  test("Part one solution works against example.") {
    SandSensor.takeReading(fs2.Stream.emits(partOneExample))
      .flatMap(result => IO(assertEquals(result, partOneSolution)))
  }

  test("Part two solution works against example.") {
    SandSensor.takeReadingInitial(fs2.Stream.emits(partOneExample))
      .flatMap(result => IO(assertEquals(result, partTwoSolution)))
  }
}
