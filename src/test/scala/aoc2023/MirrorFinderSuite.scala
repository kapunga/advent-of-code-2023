package aoc2023

class MirrorFinderSuite extends AocSuite {
  val partOneExample: List[String] = List(
    "#.##..##.",
    "..#.##.#.",
    "##......#",
    "##......#",
    "..#.##.#.",
    "..##..##.",
    "#.#.##.#.",
    "",
    "#...##..#",
    "#....#..#",
    "..##..###",
    "#####.##.",
    "#####.##.",
    "..##..###",
    "#....#..#")

  val partOneSolution: String = "405"

  val partTwoSolution: String = "400"

  test("Works with example one.") {
    runTest(MirrorFinder.findAll, partOneExample, partOneSolution)
  }

  test("Works with example two.") {
    runTest(MirrorFinder.findSmudged, partOneExample, partTwoSolution)
  }
}
