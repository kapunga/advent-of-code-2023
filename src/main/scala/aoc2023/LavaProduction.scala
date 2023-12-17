package aoc2023

import cats.effect.IO
import cats.implicits.*
import scala.annotation.tailrec

object LavaProduction {
  val allowedChars: Set[Char] = Set('.', '-', '|', '\\', '/')

  def covered(input: fs2.Stream[IO, String]): IO[String] =
    for {
      lavaChart <- toLavaChart(input)
      elemChart <- toElementChart(lavaChart)
      beamTrace =  BeamTrace(Point(0, 0), Heading.East)
    } yield processChart(beamTrace :: Nil, elemChart).toString

  def maxCovered(input: fs2.Stream[IO, String]): IO[String] =
    for {
      lavaChart <- toLavaChart(input)
      elemChart <- toElementChart(lavaChart)
    } yield findMaxCoverage(elemChart).toString
}

case class BeamTrace(loc: Point, heading: Heading)

object BeamTrace {
  def allEdges(chart: Chart[Any]): List[BeamTrace] = {
    import Heading._
    val maxX = chart.width - 1
    val maxY = chart.height - 1

    (0 to maxX).flatMap(i => BeamTrace(Point(i, maxY), North) :: BeamTrace(Point(i, 0), South) :: Nil).toList ++
      (0 to maxY).flatMap(i => BeamTrace(Point(0, i), East) :: BeamTrace(Point(maxX, i), West) :: Nil).toList
  }
}

def toLavaChart(input: fs2.Stream[IO, String]): IO[Chart[Char]] =
  input.map(_.toVector)
    .compile
    .toVector
    .flatMap(bc => {
      if (bc.consistent(LavaProduction.allowedChars)) IO.pure(bc)
      else IO.aocError("Inconsistent Lava Chart")
    })

def toElementChart(chart: Chart[Char]): IO[Chart[Element]] =
  chart.toList
    .traverse(_.toList.traverse(Element.parse))
    .map(_.map(_.toVector).toVector)

@tailrec
def processChart(traces: List[BeamTrace],
                       chart: Chart[Element],
                       visited: Set[BeamTrace] = Set.empty): Long = {
  if (traces == Nil) visited.map(_.loc).size
  else {
    val (nextTraces, _) =
      traces
        .flatMap(t => chart.at(t.loc).process(t))
        .filterNot(visited.contains)
        .partition(nt => chart.isInBounds(nt.loc))

    processChart(nextTraces, chart, visited ++ traces.toSet)
  }
}

def findMaxCoverage(chart: Chart[Element]): Long =
  BeamTrace.allEdges(chart)
    .map(bt => processChart(bt :: Nil, chart))
    .max

sealed trait Element {
  def process(bt: BeamTrace): List[BeamTrace]

  def passThrough(bt: BeamTrace): List[BeamTrace] = {
    val nl = bt.heading.nextPoint(bt.loc)
    BeamTrace(nl, bt.heading) :: Nil
  }
}
object Element {
  def parse(char: Char): IO[Element] = char match {
    case '.'  => IO.pure(Empty)
    case '|'  => IO.pure(NsSplitter)
    case '-'  => IO.pure(EwSplitter)
    case '/'  => IO.pure(NeSwMirror)
    case '\\' => IO.pure(NwSeMirror)
    case c    => IO.aocError(s"Invalid map character: '$c'")
  }

  case object Empty extends Element {
    override def process(bt: BeamTrace): List[BeamTrace] =
      passThrough(bt)
  }

  case object NsSplitter extends Element {
    override def process(bt: BeamTrace): List[BeamTrace] = {
      bt.heading match {
        case Heading.East => northSouth(bt.loc)
        case Heading.West => northSouth(bt.loc)
        case _            => passThrough(bt)
      }
    }

    private def northSouth(loc: Point): List[BeamTrace] =
      List(
        BeamTrace(Heading.North.nextPoint(loc), Heading.North),
        BeamTrace(Heading.South.nextPoint(loc), Heading.South))
  }

  case object EwSplitter extends Element {
    override def process(bt: BeamTrace): List[BeamTrace] = {
      bt.heading match {
        case Heading.North => eastWest(bt.loc)
        case Heading.South => eastWest(bt.loc)
        case _ => passThrough(bt)
      }
    }

    private def eastWest(loc: Point): List[BeamTrace] =
      List(
        BeamTrace(Heading.East.nextPoint(loc), Heading.East),
        BeamTrace(Heading.West.nextPoint(loc), Heading.West))
  }

  case object NeSwMirror extends Element {
    override def process(bt: BeamTrace): List[BeamTrace] = {
      val newHeading = bt.heading match {
        case Heading.North => Heading.East
        case Heading.East  => Heading.North
        case Heading.South => Heading.West
        case Heading.West  => Heading.South
      }

      BeamTrace(newHeading.nextPoint(bt.loc), newHeading) :: Nil
    }
  }

  case object NwSeMirror extends Element {
    override def process(bt: BeamTrace): List[BeamTrace] = {
      val newHeading = bt.heading match {
        case Heading.North => Heading.West
        case Heading.East => Heading.South
        case Heading.South => Heading.East
        case Heading.West => Heading.North
      }

      BeamTrace(newHeading.nextPoint(bt.loc), newHeading) :: Nil
    }
  }
}
