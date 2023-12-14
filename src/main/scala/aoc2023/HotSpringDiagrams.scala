package aoc2023

import cats.Show
import cats.effect.IO
import cats.implicits.*

object HotSpringDiagrams {
  def possibilities(input: fs2.Stream[IO, String]): IO[String] =
    input.evalMap(DiagramRow.parse)
      .map(_.calcPossible)
      .compile
      .toList
      .map(_.sum.toString)

  def bigPossibilities(input: fs2.Stream[IO, String]): IO[String] = {
    input.evalMap(DiagramRow.parseRepeat(5, _))
      .map(_.calcPossible)
      .compile
      .toList
      .map(_.sum.toString)
  }
}

case class DiagramRow(row: String, counts: List[Int]) {
  private type Memo = Map[(Int, List[Int]), Long]
  def calcPossible: Long = calc(0, counts, Map.empty)._1

  private def possiblePlacements(loc: Int, size: Int): List[Int] = {
    val end = row.length - size
    val nxtDmg = row.indexOf("#", loc)

    val max = if (nxtDmg != -1 && nxtDmg < end) nxtDmg else end

    (loc to max)
      .filter(l => l + size >= row.length || row(l + size) != '#')
      .filter(i => !(i until (i + size)).exists(i1 => row(i1) == '.'))
      .toList
  }

  private def calc(from: Int, groups: List[Int], memo: Memo): (Long, Memo) = {
    if (memo.contains((from, groups))) {
      (memo((from, groups)), memo)
    } else if (groups == Nil) {
      if (from == row.length || row.indexOf('#', from) == -1) (1L, memo) else (0L, memo)
    } else {
      val (tot, mem) = possiblePlacements(from, groups.head).foldLeft((0L, memo))((tuple, i) => {
        val (total1, memo1) = tuple
        val (total2, memo2) = calc(i + groups.head + 1, groups.tail, memo1)

        (total1 + total2, memo1 ++ memo2)
      })

      (tot, mem + ((from, groups) -> tot))
    }
  }
}

object DiagramRow {
  given Show[DiagramRow] with
    def show(dr: DiagramRow): String = s"${dr.row} ${dr.counts.map(_.toString).mkString(",")}"

  def parse(s: String): IO[DiagramRow] = {
    s match {
      case s"$row $counts" =>
        val validRow = row.replaceAll("[?.#]", "").isEmpty
        if (validRow) IO(DiagramRow(row, counts.split(",").toList.map(_.toInt)))
        else IO.raiseError(new Exception(s"Invalid row format: $row"))
      case _ => IO.raiseError(new Exception("Invalid String Format"))
    }
  }

  def parseRepeat(count: Int, s: String): IO[DiagramRow] = {
    for {
      small <- parse(s)
    } yield {
      val bigList = List.fill(count)(small.row).mkString("?")
      val bigCounts = List.fill(count)(small.counts).flatten
      DiagramRow(bigList, bigCounts)
    }
  }
}
