package aoc2023

class RockPlatformSuite extends AocSuite {
  val partOneExample: List[String] = List(
    "O....#....",
    "O.OO#....#",
    ".....##...",
    "OO.#O....O",
    ".O.....O#.",
    "O.#..O.#.#",
    "..O..#O..O",
    ".......O..",
    "#....###..",
    "#OO..#....")

  val partOneSolution: String = "136"

  val partTwoSolution: String = "64"

  test("Works with example one.") {
    runTest(RockPlatform.checkLoad, partOneExample, partOneSolution)
  }
  
  test("Works with example two.") {
    runTest(RockPlatform.loadTest, partOneExample, partTwoSolution)
  }
}
