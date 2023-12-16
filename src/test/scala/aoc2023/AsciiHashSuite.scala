package aoc2023

class AsciiHashSuite extends AocSuite {
  val partOneExample: List[String] =
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" :: Nil

  val partOneSolution: String = "1320"

  val partTwoSolution: String = "145"

  test("Works with example one.") {
    runTest(AsciiHash.runHash, partOneExample, partOneSolution)
  }

  test("Works with example two.") {
    runTest(AsciiHash.lensePower, partOneExample, partTwoSolution)
  }
}
