package aoc2023

class PipeMazeSuite extends AocSuite {
  val simpleBoxClean: List[String] = List(
    ".....",
    ".S-7.",
    ".|.|.",
    ".L-J.",
    ".....")

  val simpleBoxRaw: List[String] = List(
    "-L|F7",
    "7S-7|",
    "L|7||",
    "-L-J|",
    "L|-JF")

  val simpleBoxSolution: String = "4"

  val twistyPipes: List[String] = List(
    "..F7.",
    ".FJ|.",
    "SJ.L7",
    "|F--J",
    "LJ...")

  val twistyPipesSolution: String = "8"

  val simpleInterior: List[String] = List(
    "...........",
    ".S-------7.",
    ".|F-----7|.",
    ".||.....||.",
    ".||.....||.",
    ".|L-7.F-J|.",
    ".|..|.|..|.",
    ".L--J.L--J.",
    "...........")

  val simpleInteriorSolution: String = "4"

  val twistyInterior: List[String] = List(
    ".F----7F7F7F7F-7....",
    ".|F--7||||||||FJ....",
    ".||.FJ||||||||L7....",
    "FJL7L7LJLJ||LJ.L-7..",
    "L--J.L7...LJS7F-7L7.",
    "....F-J..F7FJ|L7L7L7",
    "....L7.F7||L7|.L7L7|",
    ".....|FJLJ|FJ|F7|.LJ",
    "....FJL-7.||.||||...",
    "....L---J.LJ.LJLJ...")

  val twistyInteriorSolution: String = "8"

  val noisyInterior: List[String] = List(
    "FF7FSF7F7F7F7F7F---7",
    "L|LJ||||||||||||F--J",
    "FL-7LJLJ||||||LJL-77",
    "F--JF--7||LJLJ7F7FJ-",
    "L---JF-JLJ.||-FJLJJ7",
    "|F|F-JF---7F7-L7L|7|",
    "|FFJF7L7F-JF7|JL---7",
    "7-L-JL7||F7|L7F-7F7|",
    "L.L7LFJ|||||FJL7||LJ",
    "L7JLJL-JLJLJL--JLJ.L")

  val noisyInteriorSolution: String = "10"
  
  test("Finds path in simple box shape.") {
    runTest(PipeMaze.maxLength, simpleBoxClean, simpleBoxSolution)
  }

  test("Find path in box shape with noise.") {
    runTest(PipeMaze.maxLength, simpleBoxRaw, simpleBoxSolution)
  }

  test("Find path in twisty path.") {
    runTest(PipeMaze.maxLength, twistyPipes, twistyPipesSolution)
  }

  test("Counts interior in simple shape") {
    runTest(PipeMaze.interior, simpleInterior, simpleInteriorSolution)
  }

  test("Counts interior in twisty shape") {
    runTest(PipeMaze.interior, twistyInterior, twistyInteriorSolution)
  }

  test("Counts interior in noisy shape") {
    runTest(PipeMaze.interior, noisyInterior, noisyInteriorSolution)
  }
}
