package aoc2023

import cats.effect.IO
import munit.CatsEffectSuite

class CamelDirectionsSuite extends CatsEffectSuite {
  val partOneAExample: List[String] = List(
    "RL",
    "",
    "AAA = (BBB, CCC)",
    "BBB = (DDD, EEE)",
    "CCC = (ZZZ, GGG)",
    "DDD = (DDD, DDD)",
    "EEE = (EEE, EEE)",
    "GGG = (GGG, GGG)",
    "ZZZ = (ZZZ, ZZZ)")

  val partOneASolution: String = "2"

  val partOneBExample: List[String] = List(
    "LLR",
    "",
    "AAA = (BBB, BBB)",
    "BBB = (AAA, ZZZ)",
    "ZZZ = (ZZZ, ZZZ)")

  val partOneBSolution: String = "6"

  val partTwoExample: List[String] = List(
    "LR",
    "",
    "11A = (11B, XXX)",
    "11B = (XXX, 11Z)",
    "11Z = (11B, XXX)",
    "22A = (22B, XXX)",
    "22B = (22C, 22C)",
    "22C = (22Z, 22Z)",
    "22Z = (22B, 22B)",
    "XXX = (XXX, XXX)")

  val partTwoSolution: String = "6"

  test("Part one a solution works against example.") {
    CamelDirections.follow(fs2.Stream.emits(partOneAExample))
      .flatMap(result => IO(assertEquals(result, partOneASolution)))
  }

  test("Part one b solution works against example.") {
    CamelDirections.follow(fs2.Stream.emits(partOneBExample))
      .flatMap(result => IO(assertEquals(result, partOneBSolution)))
  }

  test("Part two solution works against example.") {
    CamelDirections.followAll(fs2.Stream.emits(partTwoExample))
      .flatMap(result => IO(assertEquals(result, partTwoSolution)))
  }
}
