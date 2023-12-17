package aoc2023

import cats.effect.IO

type Chart[T] = Vector[Vector[T]]

case class AocError(msg: String) extends Throwable(msg)

case class Point(x: Int, y: Int)

enum Heading {
  case North, East, South, West
}

extension (s: String)
  def toNumberList: IO[List[Long]] =
    IO(s.split("\\s+").map(_.toLong).toList)

extension [T](chart: Chart[T])
  def consistent(set: Set[T]): Boolean =
    chart.headOption.forall(h => chart.forall(_.size == h.size)) &&
      chart.forall(_.forall(set.contains))
  def width: Int = chart.head.size
  def height: Int = chart.size
  def render: String = chart.map(_.mkString).mkString("\n")
  def at(p: Point): T = chart(p.y)(p.x)
  def isInBounds(p: Point): Boolean =
    p.x >= 0 && p.x < width && p.y >= 0 && p.y < height

extension (h: Heading)
  def nextPoint(p: Point): Point =
    h match {
      case Heading.North => Point(p.x, p.y - 1)
      case Heading.East  => Point(p.x + 1, p.y)
      case Heading.South => Point(p.x, p.y + 1)
      case Heading.West  => Point(p.x - 1, p.y)
    }

extension (io: IO.type)
  def aocError(msg: String): IO[Nothing] = io.raiseError(AocError(msg))
