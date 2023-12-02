package aoc2023

import cats.effect.IO
import fs2.Pipe
import cats.implicits.*
import cats.kernel.CommutativeMonoid

object CubeGame {
  def partOneDraw: Draw = Draw(red = 12, green = 13, blue = 14)

  def solve(d: Draw)(input: fs2.Stream[IO, String]): IO[String] = {
    val s = input.through(parseGames).filter(_.possible(d)).map(_.id)

    result(s)
  }

  def power(input: fs2.Stream[IO, String]): IO[String] = {
    val s = input.through(parseGames).map(_.power)

    result(s)
  }

  private def parseGames: Pipe[IO, String, Game] =
    _.evalMap(l => IO.fromOption(Game.parse(l))(new RuntimeException("Unable to create game.")))

  private def result(s: fs2.Stream[IO, Long]): IO[String] =
    s.compile.fold(0L)((a, b) => a + b).map(_.toString)
}

case class Draw(red: Int, blue: Int, green: Int) {
  def compatible(other: Draw): Boolean =
    red <= other.red && blue <= other.blue && green <= other.green

  def power: Long = red.toLong * blue.toLong * green.toLong
}

object Draw {
  given CommutativeMonoid[Draw] with
    override def empty: Draw = Draw(0, 0, 0)
    override def combine(x: Draw, y: Draw): Draw =
      Draw(
        red = math.max(x.red, y.red),
        blue = math.max(x.blue, y.blue),
        green = math.max(x.green, y.green))

  def parseDraw(s: String): Option[Draw] = {
    val results: Option[List[Draw]] =
      s.split(", ").toList.traverse {
        case s"$n red" => Some(Draw(n.toInt, 0, 0))
        case s"$n blue" => Some(Draw(0, n.toInt, 0))
        case s"$n green" => Some(Draw(0, 0, n.toInt))
        case _ => None
      }

    results.map(_.unorderedFold)
  }
}

case class Game(id: Long, draws: List[Draw]) {
  def possible(draw: Draw): Boolean = draws.unorderedFold.compatible(draw)
  def power: Long = draws.unorderedFold.power
}

object Game {
  def parse(s: String): Option[Game] =
    s match {
      case s"Game $id: $hands" =>
        hands.split("; ").map(Draw.parseDraw).toList
          .sequence.map(d => Game(id.toInt, d))
      case _ => None
    }
}
