package aoc2023

import cats.effect.IO
import scala.annotation.tailrec

object RockPlatform {
  def checkLoad(input: fs2.Stream[IO, String]): IO[String] =
    input.compile.toList.map(calcNorthLoad.compose(rockNorth)).map(_.toString)

  def loadTest(input: fs2.Stream[IO, String]): IO[String] =
    input.compile.toList.map(sl => cycleN(sl, 1000000000)).map(calcNorthLoad).map(_.toString)

  def calcNorthLoad(rows: List[String]): Long = {
    @tailrec
    def load(s: String, acc: Long = 0): Long = {
      if (s == "") acc
      else {
        val l = if (s.head == 'O') s.length else 0
        load(s.tail, acc + l)
      }
    }

    diagonalRotate(rows).map(load(_)).sum
  }

  def diagonalRotate(rows: List[String]): List[String] =
    (0 until rows.head.length).map(i => rows.map(_(i)).mkString).toList

  def rockNorth(rows: List[String]): List[String] = {
    val newRows = diagonalRotate(rows)

    val len = newRows.head.length

    val rocked =
      newRows.map(_.split("#")
        .map(_.toList.sorted.reverse.mkString)
        .mkString("#")
        .padTo(len, '#'))

    diagonalRotate(rocked)
  }

  @tailrec
  def cycleN(rows: List[String], count: Long, stringMap: Map[List[String], Long] = Map.empty): List[String] = {
    if (count == 0) rows
    else if (stringMap.contains(rows)) {
      val cycleLength = stringMap(rows) - count

      val shortCount = count % cycleLength

      println(s"Cutting down from $count -> $shortCount")

      cycleN(rows, shortCount)
    } else {
      val cycled = cycle(rows)

      cycleN(cycled, count - 1, stringMap + (rows -> count))
    }
  }


  def rotate90(rows: List[String]): List[String] =
    diagonalRotate(rows).map(_.reverse)

  def cycle(rows: List[String]): List[String] = {
    val r1 = rockNorth.andThen(rotate90)(rows)
    val r2 = rockNorth.andThen(rotate90)(r1)
    val r3 = rockNorth.andThen(rotate90)(r2)
    rockNorth.andThen(rotate90)(r3)
  }
}

extension (ls: List[String])
  def render: String = "\n" + ls.mkString("\n")
