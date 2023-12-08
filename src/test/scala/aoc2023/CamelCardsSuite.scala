package aoc2023

import cats.effect.IO
import munit.CatsEffectSuite

class CamelCardsSuite extends CatsEffectSuite {
  val partOneExample: List[String] = List(
    "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483")

  val partOneSolution: String = "6440"
  
  val partTwoSolution: String = "5905"

  test("Part one solution works against example.") {
    CamelCards.winnings(fs2.Stream.emits(partOneExample))
      .flatMap(result => IO(assertEquals(result, partOneSolution)))
  }

  test("Part two solution works against example.") {
    CamelCards.wildWinnings(fs2.Stream.emits(partOneExample))
      .flatMap(result => IO(assertEquals(result, partTwoSolution)))
  }
}
