package aoc2023

import cats.effect.IO
import scala.annotation.tailrec

object SandSensor {
  def takeReading(input: fs2.Stream[IO, String]): IO[String] = {
    input
      .evalMap(_.toNumberList)
      .map(read)
      .map(_._2)
      .compile
      .fold(0L)(_ + _)
      .map(_.toString)
  }

  def takeReadingInitial(input: fs2.Stream[IO, String]): IO[String] = {
    input
      .evalMap(_.toNumberList)
      .map(read)
      .map(_._1)
      .compile
      .fold(0L)(_ + _)
      .map(_.toString)
  }

  private def read(readings: List[Long]): (Long, Long) = readImpl(readings, readings.head)

  @tailrec
  private def readImpl(readings: List[Long], head: Long, acc: List[Long] = Nil): (Long, Long) = {
    readings match {
      case Nil => throw new Exception("WTF Bro???")
      case first :: second :: _ => readImpl(readings.tail, head, (second - first) :: acc)
      case last :: Nil =>
        if (acc.take(2) == List(0, 0)) {
          (last, last)
        } else {
          val (front, back) = read(acc.reverse)
          (head - front, back + last)
        }
    }
  }
}
