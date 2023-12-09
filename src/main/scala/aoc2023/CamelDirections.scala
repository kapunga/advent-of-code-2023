package aoc2023

import cats.effect.IO
import cats.effect.std.Console
import cats.implicits.*
import scala.annotation.tailrec

object CamelDirections {
  def follow(input: fs2.Stream[IO, String]): IO[String] = {
    for {
      dng <- directionsAndGuide(input)
      (directions, guide) = dng
      steps <- followDirections(directions, guide, "AAA", s => s != "ZZZ")
    } yield steps.toString
  }

  def followAll(input: fs2.Stream[IO, String]): IO[String] = {
    for {
      dng <- directionsAndGuide(input)
      (directions, guide) = dng
      steps <- followAllDirections(directions, guide)
    } yield steps.toString
  }

  private def directionsAndGuide(input: fs2.Stream[IO, String]): IO[(String, Map[String, (String, String)])] =
    for {
      directions <- input.take(2).compile.toList.map(_.head)
      guide <- input.drop(2).evalMap {
        case s"$node = ($left, $right)" => IO.pure(node -> (left, right))
        case row => IO.raiseError(new Exception(s"Invalid row: $row"))
      }.compile.toList.map(_.toMap)
    } yield (directions, guide)

  private def followDirections(dir: String, guide: Map[String, (String, String)], start: String, endState: String => Boolean): IO[Long] = {
    val directionStream = fs2.Stream.emits(dir.toList).repeat

    directionStream.evalScan((start, 0L))((t, dir) => {
      val (loc, count) = t
      guide.get(loc) match {
        case Some((left, right)) =>
          if (loc == left && loc == right) IO.raiseError(new Exception(s"Stuck in terminal location $loc"))
          else if (dir == 'L') IO.pure((left, count + 1))
          else if (dir == 'R') IO.pure((right, count + 1))
          else IO.raiseError(new Exception(s"Invalid direction: $dir"))
        case None => IO.raiseError(new Exception(s"No entry for $loc in $guide"))
      }
    }).takeThrough((loc, _) => endState(loc))
      .compile
      .last
      .map(_.map(_._2).getOrElse(0L))
  }

  private def followAllDirections(dir: String, guide: Map[String, (String, String)]): IO[Long] = {
    val startList = guide.keys.filter(_.endsWith("A")).toList

    for {
      stepsList <- startList.traverse(start => followDirections(dir, guide, start, s => !s.endsWith("Z")))
      _ <- Console[IO].println(stepsList)
    } yield leastCommonMultiple(stepsList)
  }

  @tailrec
  private def primeFactor(n: Long, current: Long = 2, acc: List[Long] = Nil): List[Long] = {
    if (n == current) (current :: acc).reverse
    else if (n % current == 0) primeFactor(n / current, current, current :: acc)
    else primeFactor(n, current + 1, acc)
  }

  private def lcm(factors: List[List[Long]]): Long = {
    val maps = factors.map(f => f.groupBy(k => k).toList.map((k, v) => k -> v.length).toMap)

    val keys = maps.map(_.keySet).reduce(_ ++ _).toList

    val maxes = keys.map(k => k -> maps.map(_.getOrElse(k, 0)).max)

    maxes.map((base, exp) => math.pow(base.toDouble, exp.toDouble).toLong).product
  }

  private def leastCommonMultiple(list: List[Long]): Long = lcm(list.map(num => primeFactor(num)))
}
