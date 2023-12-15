package aoc2023

import cats.Show
import cats.effect.IO
import cats.effect.std.Console
import scala.annotation.tailrec

object MirrorFinder {
  def findAll(input: fs2.Stream[IO, String]): IO[String] =
    input.scan[(List[RockPattern], List[String])](List.empty, List.empty)((tup, s) => {
      val (rpl, il) = tup

      if (s == "") (RockPattern(il.reverse) :: rpl, List.empty)
      else (rpl, s :: il)
    })
      .compile
      .last
      .map(t => {
        val (rp, sl) = t.get
        RockPattern(sl.reverse) :: rp
      })
      .map(_.map(_.count).sum.toString)

  def findSmudged(input: fs2.Stream[IO, String]): IO[String] =
    input.scan[(List[RockPattern], List[String])](List.empty, List.empty)((tup, s) => {
        val (rpl, il) = tup

        if (s == "") (RockPattern(il.reverse) :: rpl, List.empty)
        else (rpl, s :: il)
      })
      .compile
      .last
      .map(t => {
        val (rp, sl) = t.get
        RockPattern(sl.reverse) :: rp
      })
      .map(_.map(_.countSmudged).sum.toString)
}

case class RockPattern(rows: List[String]) {
  @tailrec
  private def splitEven(front: List[String], back: List[String] = List.empty): Option[Long] = {
    if (front.isEmpty) None
    else if (back.nonEmpty && front.head == back.head) {
      if (front.zip(back).forall(_ == _)) Some(back.length)
      else splitEven(front.tail, front.head :: back)
    } else splitEven(front.tail, front.head :: back)
  }

  @tailrec
  private def splitSmudged(front: List[String], back: List[String] = List.empty): Option[Long] = {
    def compareSmudged(a: String, b: String): Int =
      a.zip(b).map(_ == _).count(_ == false)

    if (front.isEmpty) None
    else if (back.nonEmpty && compareSmudged(front.head, back.head) <= 1) {
      if (front.zip(back).map(compareSmudged).sum == 1) Some(back.length)
      else splitSmudged(front.tail, front.head :: back)
    } else splitSmudged(front.tail, front.head :: back)
  }

  def count: Long = {
    splitEven(rows) match {
      case Some(n) => 100 * n
      case None =>
        splitEven(diagonalRotate) match {
          case Some(n) => n
          case None => throw new RuntimeException("No mirrorpoint found.")
        }
    }
  }

  def countSmudged: Long = {
    splitSmudged(rows) match {
      case Some(n) => 100 * n
      case None =>
        splitSmudged(diagonalRotate) match {
          case Some(n) => n
          case None => throw new RuntimeException("No mirrorpoint found.")
        }
    }
  }

  private def diagonalRotate: List[String] =
    (0 until rows.head.length).map(i => rows.map(_(i)).mkString).toList
}

object RockPattern {
  given Show[RockPattern] with
    def show(dr: RockPattern): String = "\n" + dr.rows.mkString("\n")
}
