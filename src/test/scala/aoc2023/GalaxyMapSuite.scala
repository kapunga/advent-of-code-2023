package aoc2023

class GalaxyMapSuite extends AocSuite {
  val partOneExample: List[String] = List(
    "...#......",
    ".......#..",
    "#.........",
    "..........",
    "......#...",
    ".#........",
    ".........#",
    "..........",
    ".......#..",
    "#...#.....")

  val partOneSolution: String = "374"

  val tenXSolution: String = "1030"

  val hundredXSolution: String = "8410"

  test("Part One Solution works with example") {
    runTest(GalaxyMap.distances(2, _), partOneExample, partOneSolution)
  }

  test("10X Solution works with example") {
    runTest(GalaxyMap.distances(10, _), partOneExample, tenXSolution)
  }

  test("100X Solution works with example") {
    runTest(GalaxyMap.distances(100, _), partOneExample, hundredXSolution)
  }
}
