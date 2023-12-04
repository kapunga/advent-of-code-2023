package aoc2023

import cats.effect.IO
import munit.CatsEffectSuite

class EngineSchematicSuite extends CatsEffectSuite {
  val partOneExample: List[String] = List(
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598..")

  val partOneSolution: String = "4361"
  
  val partTwoSolution: String = "467835"

  test("Part one solution works against example.") {
    EngineSchematic.partNumbers(fs2.Stream.emits(partOneExample))
      .flatMap(result => IO(assertEquals(result, partOneSolution)))
  }

  test("Part two solution works against example.") {
    EngineSchematic.gearRatios(fs2.Stream.emits(partOneExample))
      .flatMap(result => IO(assertEquals(result, partTwoSolution)))
  }
}
