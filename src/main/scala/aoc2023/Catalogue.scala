package aoc2023

import cats.effect.IO
import scala.annotation.tailrec

object Catalogue {
  def lookup(input: fs2.Stream[IO, String]): IO[String] = lookupImpl(Start, input)

  def lookupRange(input: fs2.Stream[IO, String]): IO[String] = lookupImpl(RangeStart, input)

  private def lookupImpl(startState: MappingState, input: fs2.Stream[IO, String]): IO[String] =
    input.evalMap(Entry.apply)
      .evalScan[IO, MappingState](startState)((ms, entry) =>
        ms.process(entry))
      .compile
      .last
      .flatMap(_.get.minId)
      .map(_.toString)
}

sealed trait Entry

case class Seeds(seeds: List[Long]) extends Entry
case class Section(from: String, to: String) extends Entry
case class Mapping(start: Long, length: Long, offset: Long) extends Entry
case object Complete extends Entry

object Entry {
  def apply(entry: String): IO[Entry] = entry match {
    case s"seeds: $seeds" =>
      IO(seeds.split(" ").map(_.toLong))
        .map(s => Seeds(s.toList))
    case s"$from-to-$to map:" => IO.pure(Section(from, to))
    case s"$source $dest $step" =>
      IO(Mapping(dest.toLong, step.toLong, source.toLong - dest.toLong))
    case "" => IO.pure(Complete)
    case invalid => IO.raiseError(new RuntimeException(s"Invalid entry: '$invalid'"))
  }
}

case class StateError(entry: Entry, state: MappingState) extends Exception(s"Invalid entry '$entry' for state '$state'")
case class MapError(category: String, from: String) extends Exception(s"Invalid category mapping start '$from', should be '$category'")

sealed trait MappingState {
  def process(entry: Entry): IO[MappingState]
  def minId: IO[Long]
}

// The following states are used to lookup individual seed ids
case object Start extends MappingState {
  override def process(entry: Entry): IO[MappingState] =
    entry match
      case Seeds(seeds) => IO.pure(Category("seed", seeds))
      case _ => IO.raiseError(StateError(entry, this))
  override def minId: IO[Long] =
    IO.raiseError(new Exception("Can't call `minId` on 'Start'."))
}
case class Category(name: String, ids: List[Long]) extends MappingState {
  override def process(entry: Entry): IO[MappingState] =
    entry match
      case Section(from, to) =>
        if (from != name) IO.raiseError(MapError(name, from))
        else IO.pure(PendingMap(from, to, ids, Nil))
      case Complete =>
        if (name == "seed") IO.pure(this)
        else IO.raiseError(StateError(entry, this))
      case _ => IO.raiseError(StateError(entry, this))

  override def minId: IO[Long] = IO.pure(ids.min)
}
case class PendingMap(from: String, to: String, pending: List[Long], done: List[Long]) extends MappingState {
  override def process(entry: Entry): IO[MappingState] =
    entry match
      case Complete => IO.pure(Category(to, pending ++ done))
      case Mapping(start, length, offset) =>
        val (toUpdate, rest) = pending.partition(i => i >= start && i < start + length)
        val updated = toUpdate.map(_ + offset)
        IO.pure(PendingMap(from, to, rest, done ++ updated))
      case _ => IO.raiseError(StateError(entry, this))

  override def minId: IO[Long] = IO.pure((pending ++ done).min)
}

case class Range(start: Long, length: Long) {
  def split(os: Long, ol: Long): List[Range] = {
    val split1 = if (start < os && start + length >= os) Some(os) else None
    val split2 = if (start < os + ol && start + length >= os + ol) Some(os + ol) else None

    (split1, split2) match {
      case (Some(a), Some(b)) =>
        List(
          Range(start, a - start),
          Range(a, b - a),
          Range(b, (start + length) - b))
      case (Some(a), None) =>
        List(
          Range(start, a - start),
          Range(a, (start + length) - a))
      case (None, Some(b)) =>
        List(
          Range(start, b - start),
          Range(b, (start + length) - b))
      case (None, None) => List(this)
    }
  }

  def lookup(offset: Long): Range =
    Range(start + offset, length)
}

// The following states are used to lookup ranges of seed ids
case object RangeStart extends MappingState {
  override def process(entry: Entry): IO[MappingState] =
    entry match
      case Seeds(seeds) => IO.pure(RangedCategory("seed", ranges(seeds)))
      case _ => IO.raiseError(StateError(entry, this))

  override def minId: IO[Long] =
    IO.raiseError(new Exception("Can't call `minId` on 'Start'."))

  @tailrec
  private def ranges(raw: List[Long], acc: List[Range] = Nil): List[Range] = {
    if (raw.isEmpty) acc.reverse
    else {
      raw match {
        case start :: length :: rest => ranges(rest, Range(start, length) :: acc)
        case _ => throw new RuntimeException(s"Odd number of seed inputs")
      }
    }
  }
}

case class RangedCategory(name: String, ranges: List[Range]) extends MappingState {
  override def process(entry: Entry): IO[MappingState] =
    entry match
      case Section(from, to) =>
        if (from != name) IO.raiseError(MapError(name, from))
        else IO.pure(PendingRangeMap(from, to, ranges, Nil))
      case Complete =>
        if (name == "seed") IO.pure(this)
        else IO.raiseError(StateError(entry, this))
      case _ => IO.raiseError(StateError(entry, this))

  override def minId: IO[Long] = IO.pure(ranges.map(_.start).min)
}

case class PendingRangeMap(from: String, to: String, pending: List[Range], done: List[Range]) extends MappingState {
  override def process(entry: Entry): IO[MappingState] =
    entry match
      case Complete => IO.pure(RangedCategory(to, pending ++ done))
      case Mapping(start, length, offset) =>
        val (d, p) = processRange(start, length)
        val updated = d.map(_.lookup(offset))
        IO.pure(PendingRangeMap(from, to, p, done ++ updated))
      case _ => IO.raiseError(StateError(entry, this))

  private def processRange(start: Long, length: Long): (List[Range], List[Range]) = {
    val split = pending.flatMap(_.split(start, length))
    split.partition(r => r.start >= start && r.start + r.length <= start + length)
  }

  override def minId: IO[Long] = IO.pure((pending ++ done).map(_.start).min)
}
