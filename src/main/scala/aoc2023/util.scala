package aoc2023

import cats.effect.IO

extension (s: String)
  def toNumberList: IO[List[Long]] =
    IO(s.split("\\s+").map(_.toLong).toList)