package aoc2023

import cats.effect.IO
import cats.effect.std.Console
import scala.annotation.tailrec

object PipeMaze {
  def maxLength(input: fs2.Stream[IO, String]): IO[String] = {
    toMaze(input).flatMap(_.maxlength).map(_.toString)
  }

  def interior(input: fs2.Stream[IO, String]): IO[String] = {
    toMaze(input).flatMap(_.countInterior).map(_.toString)
  }

  private def toMaze(input: fs2.Stream[IO, String]): IO[Maze] =
    input.map(_.toVector).compile.toVector.flatMap(Maze.apply)
}

case class Coord(x: Int, y: Int) {
  def adjacent(dim: Dimensions): List[Coord] =
    List(left, right, up, down)
      .filterNot(c => c.x < 0 || c.x >= dim.x || c.y < 0 || c.y >= dim.y)

  def left: Coord = Coord(x - 1, y)
  def right: Coord = Coord(x + 1, y)
  def up: Coord = Coord(x, y - 1)
  def down: Coord = Coord(x, y + 1)

  override def toString: String = s"[$x, $y]"
}

type Dimensions = Coord

class Maze(start: Coord, dimensions: Dimensions, chart: Chart) {
  def charAt(p: Coord): Char = chart(p.y)(p.x)

  def findCircuit: IO[List[Coord]] = {
    val adj = start.adjacent(dimensions)

    val circuit = adj.map(c => circuitImpl(c, start, start :: Nil)).flatten.headOption

    IO.fromOption(circuit)(new Exception("No circuit found!"))
  }

  @tailrec
  final def circuitImpl(at: Coord, begin: Coord, path: List[Coord]): Option[List[Coord]] = {
    val next = nextCoord(at, path.head)

    if (next.isEmpty) None
    else if (next.contains(begin)) Some(at :: path)
    else circuitImpl(next.get, begin, at :: path)
  }

  def nextCoord(now: Coord, prev: Coord): Option[Coord] = {
    val valid = validAdjacent(now)

    now.adjacent(dimensions).filterNot(_ == prev).filter(valid.contains) match {
      case coord :: Nil => Option.when(validAdjacent(coord).contains(now))(coord)
      case _ => None
    }
  }

  def maxlength: IO[Int] = findCircuit.map(_.length / 2)

  private def validAdjacent(c: Coord): Set[Coord] = {
    charAt(c) match {
      case '-' => Set(Coord(c.x - 1, c.y), Coord(c.x + 1, c.y))
      case '|' => Set(Coord(c.x, c.y - 1), Coord(c.x, c.y + 1))
      case 'F' => Set(Coord(c.x + 1, c.y), Coord(c.x, c.y + 1))
      case '7' => Set(Coord(c.x - 1, c.y), Coord(c.x, c.y + 1))
      case 'L' => Set(Coord(c.x + 1, c.y), Coord(c.x, c.y - 1))
      case 'J' => Set(Coord(c.x - 1, c.y), Coord(c.x, c.y - 1))
      case 'S' => Set(Coord(c.x - 1, c.y), Coord(c.x + 1, c.y), Coord(c.x, c.y - 1), Coord(c.x, c.y + 1))
      case _   => Set.empty
    }
  }

  def countInterior: IO[Int] = {
    for {
      path <- findCircuit.map(_.toSet)
    } yield countInteriorImpl(path)
  }

  def startChar(path: Set[Coord]): Char = {
    val coords = start.adjacent(dimensions).filter(path.contains).toSet

    val pointsRight = Set('-', 'L', 'F')
    val pointsLeft = Set('-', 'J', '7')
    val pointsDown = Set('|', 'F', '7')

    if (coords.contains(start.left) && pointsRight.contains(charAt(start.left))) {
      if (coords.contains(start.up) && pointsDown.contains(charAt(start.up))) 'J'
      else if (coords.contains(start.right) && pointsLeft.contains(charAt(start.right))) '-'
      else '7'
    } else if (coords.contains(start.up) && pointsDown.contains(charAt(start.up))) {
      if (coords.contains(start.right) && pointsLeft.contains(charAt(start.right))) 'L'
      else '|'
    } else 'F'
  }

  def countInteriorImpl(path: Set[Coord]): Int = {
    val s = startChar(path)

    def isInterior(c: Coord) = {
      if (path.contains(c)) false
      else {
        val xbuf = scala.Range(0, c.x)
          .map(x => Coord(x, c.y))
          .filter(path.contains)
          .map(charAt)
          .mkString

        val ybuf = scala.Range(0, c.y)
          .map(y => Coord(c.x, y))
          .filter(path.contains)
          .map(charAt)
          .mkString

        val xCleaned =
          xbuf
            .replaceAll("S", s.toString)
            .replaceAll("-", "")
            .replaceAll("FJ|L7", "|")
        val yCleaned =
          ybuf
            .replaceAll("S", s.toString)
            .replaceAll("\\|", "")
            .replaceAll("FJ|7L", "-")

        xCleaned.count(_ == '|') % 2 == 1 && yCleaned.count(_ == '-') % 2 == 1
      }
    }

    val interior = for {
      x <- scala.Range(0, dimensions.x)
      y <- scala.Range(0, dimensions.y)
    } yield isInterior(Coord(x, y))

    interior.count(_ == true)
  }
}

object Maze {
  val validChars: Set[Char] = Set('.', '-', '|', 'F', '7', 'J', 'L', 'S')
  def apply(chart: Vector[Vector[Char]]): IO[Maze] = {
    if (chart.isEmpty || chart.head.isEmpty) {
      IO.raiseError(new Exception(s"Can't construct a maze from an empty chart!"))
    } else {
      val dimensions = Coord(chart.head.length, chart.length)

      val consistent = chart.forall(_.forall(validChars.contains)) && chart.forall(_.length == dimensions.x)

      val s: Option[Coord] = chart.zipWithIndex
        .find((v, _) => v.contains('S'))
        .map((v, y) => Coord(v.indexOf('S'), y))

      if (consistent) {
        IO.fromOption(s.map(start => new Maze(start, dimensions, chart)))(new Exception(s"No starting point on chart:\n${chart.render}"))
      } else {
        IO.raiseError(new Exception(s"Chart is inconsistent:\n${chart.render}"))
      }
    }
  }
}
