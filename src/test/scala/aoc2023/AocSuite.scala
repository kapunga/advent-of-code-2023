package aoc2023

import cats.effect.IO
import munit.CatsEffectSuite

trait AocSuite extends CatsEffectSuite {
  def runTest(f: fs2.Stream[IO, String] => IO[String], input: List[String], expected: String): IO[Unit] =
    f(fs2.Stream.emits(input)).flatMap(actual => IO(assertEquals(actual, expected)))
}
