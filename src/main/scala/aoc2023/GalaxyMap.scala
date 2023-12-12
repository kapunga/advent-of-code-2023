package aoc2023

import cats.effect.IO
import scala.annotation.tailrec

object GalaxyMap {
  def distances(spacing: Long, input: fs2.Stream[IO, String]): IO[String] =
    input.evalScan(GalaxyChartBuilder())((gcb, row) => gcb.newRow(row))
      .compile
      .last
      .flatMap(gcbo => IO.fromOption(gcbo)(new Exception("Failed to produce galaxy chart")))
      .map(_.build(spacing))
      .map(gc => pairDistancesSum(gc.galaxies))
      .map(_.toString)

  def pairDistancesSum(galaxies: List[Galaxy], acc: Long = 0): Long = {
    if (galaxies == Nil) acc
    else {
      val sum = galaxies.tail.map(galaxies.head.distance).sum
      pairDistancesSum(galaxies.tail, acc + sum)
    }
  }
}

case class GalaxyChartBuilder(galaxies: List[Galaxy] = Nil, x: Int = 0, y: Int = 0) {
  def newRow(row: String): IO[GalaxyChartBuilder] = {
    @tailrec
    def parseGalaxies(r: String = row, acc: List[Galaxy] = Nil): List[Galaxy] = {
      if (r.forall(_ == '.')) acc
      else {
        val i = r.indexOf('#')
        val g = Galaxy(galaxies.length + acc.length, i, y)
        val nr = r.replaceFirst("#", ".")
        parseGalaxies(nr, g :: acc)
      }
    }

    if (row.exists(r => r != '.' && r != '#')) {
      IO.raiseError(new Exception(s"Row has invalid characters: '$row'"))
    } else if (y == 0) {
      IO.pure(GalaxyChartBuilder(parseGalaxies() ++ galaxies, row.length, y + 1))
    } else if (row.length != x) {
      IO.raiseError(new Exception(s"Inconsistent row length, $row is not $x long"))
    } else {
      IO.pure(GalaxyChartBuilder(parseGalaxies() ++ galaxies, x, y + 1))
    }
  }

  def build(spacing: Long = 2): GalaxyChart = {
    val xGaps = scala.Range(0, x).toSet -- galaxies.map(_.x.toInt).toSet
    val yGaps = scala.Range(0, y).toSet -- galaxies.map(_.y.toInt).toSet

    val newX = x + xGaps.size
    val newY = y + yGaps.size

    val newG = galaxies.map({ case Galaxy(id, gx, gy) =>
      val nx = (xGaps.count(_ < gx) * (spacing - 1)) + gx
      val ny = (yGaps.count(_ < gy) * (spacing - 1)) + gy

      Galaxy(id, nx, ny)
    })

    GalaxyChart(newG, newX, newY)
  }
}

case class GalaxyChart(galaxies: List[Galaxy], x: Long, y: Long)

case class Galaxy(id: Int, x: Long, y: Long) {
  def distance(other: Galaxy): Long = math.abs(x - other.x) + math.abs(y - other.y)
}
