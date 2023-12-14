package aoc2023

class HotSpringsDiagramsSuite extends AocSuite {
  val exampleOne: List[String] = List(
    "???.### 1,1,3",
    ".??..??...?##. 1,1,3",
    "?#?#?#?#?#?#?#? 1,3,1,6",
    "????.#...#... 4,1,1",
    "????.######..#####. 1,6,5",
    "?###???????? 3,2,1")

  val exampleOneSolution: String = "21"

  val exampleTwoSolution: String = "525152"

  test("Works with example one.") {
    runTest(HotSpringDiagrams.possibilities, exampleOne, exampleOneSolution)
  }

  test("Works with example two.") {
    runTest(HotSpringDiagrams.bigPossibilities, exampleOne, exampleTwoSolution)
  }
}
