package aoc2023

class LavaProductionSuite extends AocSuite {
  val partOneExample: List[String] = List(
    ".|...\\....",
    "|.-.\\.....",
    ".....|-...",
    "........|.",
    "..........",
    ".........\\",
    "..../.\\\\..",
    ".-.-/..|..",
    ".|....-|.\\",
    "..//.|....")

  val partOneSolution: String = "46"

  val partTwoSolution: String = "51"

  test("Part One Solution works correctly.") {
    runTest(LavaProduction.covered, partOneExample, partOneSolution)
  }

  test("Part Two Solution works correctly.") {
    runTest(LavaProduction.maxCovered, partOneExample, partTwoSolution)
  }
}
