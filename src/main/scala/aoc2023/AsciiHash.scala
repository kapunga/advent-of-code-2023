package aoc2023

import cats.effect.IO
import cats.implicits.*

object AsciiHash {
  def runHash(input: fs2.Stream[IO, String]): IO[String] =
    input.flatMap(s => fs2.Stream.emits(s.split(',').toList))
      .map(_.doAsciiHash)
      .compile
      .fold(0L)(_ + _)
      .map(_.toString)

  def lensePower(input: fs2.Stream[IO, String]): IO[String] =
    input.flatMap(s => fs2.Stream.emits(s.split(',').toList))
      .scan(LenseBoxes.empty)((lb, i) => lb.action(i))
      .compile
      .last
      .map(_.get.calc.toString)
}

case class Lense(label: String, power: Long)

case class LenseBoxes(boxes: Vector[List[Lense]]) {
  def action(a: String): LenseBoxes = a match {
    case s"$label=$power" =>
      val i = label.doAsciiHash
      val lenses = boxes(i)
      val newLense = Lense(label, power.toInt)
      val updatedLenses = appendOrUpdateLenses(lenses, newLense)
      LenseBoxes(boxes.updated(i, updatedLenses))
    case s"$label-"       =>
      val i = label.doAsciiHash
      val lenses = boxes(i)
      LenseBoxes(boxes.updated(i, lenses.filterNot(_.label == label)))
    case i => throw new RuntimeException(s"Malformed Instruction: $i")
  }

  private def appendOrUpdateLenses(lenses: List[Lense], newLense: Lense): List[Lense] =
    if (lenses.exists(_.label == newLense.label))
      lenses.map(l => if (l.label == newLense.label) newLense else l)
    else
      newLense :: lenses

  def calc: Long = {
    def calcBox(ll: List[Lense]): Long =
      ll.zipWithIndex.map((l, i) => l.power * (ll.length - i)).sum

    boxes.indices.map(i => {
      calcBox(boxes(i)) * (i + 1)
    }).sum
  }
}

object LenseBoxes {
  def empty = LenseBoxes(Vector.fill(256)(List.empty))
}

extension (s: String)
  def doAsciiHash: Int = s.foldLeft(0)((h, c) => ((h + c.toInt) * 17) % 256)
