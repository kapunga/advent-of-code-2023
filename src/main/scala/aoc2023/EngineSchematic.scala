package aoc2023

import cats.effect.IO
import scala.annotation.tailrec

object EngineSchematic {
  def partNumbers(diagram: fs2.Stream[IO, String]): IO[String] = {
    for {
      partsNums <- partsAndNums(diagram)
    } yield findPartNums.tupled(partsNums).map(_.num.toLong).sum.toString
  }

  def gearRatios(diagram: fs2.Stream[IO, String]): IO[String] =
    for {
      partsNums <- partsAndNums(diagram)
    } yield gearRatios.tupled(partsNums).toString

  private def partsAndNums(diagram: fs2.Stream[IO, String]): IO[(List[Part], List[Num])] =
    diagram
      .zipWithIndex
      .map({ case (s, i) => parseRow(i.toInt, s) })
      .compile
      .fold[(List[Part], List[Num])]((Nil, Nil))({ case ((parts, nums), (p, n)) => (parts ++ p, nums ++ n) })

  @tailrec
  def parseRow(rowNum: Int, row: String, index: Int = 0, parts: List[Part] = Nil, nums: List[Num] = Nil): (List[Part], List[Num]) = {
    if (row.isEmpty) (parts.reverse, nums.reverse)
    else if (row.head == '.') parseRow(rowNum, row.tail, index + 1, parts, nums)
    else if (!row.head.isDigit) parseRow(rowNum, row.tail, index + 1, Part(row.head, Point(rowNum, index)) :: parts, nums)
    else {
      val n = row.takeWhile(_.isDigit)
      val num = Num(n, Point(rowNum, index))
      parseRow(rowNum, row.substring(n.length), index + n.length, parts, num :: nums)
    }
  }

  def findPartNums(parts: List[Part], nums: List[Num]): List[Num] = findPartNumsImpl(parts, nums, Nil)

  @tailrec
  def findPartNumsImpl(parts: List[Part], nums: List[Num], acc: List[Num]): List[Num] = {
    if (nums.isEmpty) acc.reverse
    else {
      val num = nums.head
      val possibleParts = parts.dropWhile(_.loc.x < num.loc.x - 1)
      val candidateParts = possibleParts.takeWhile(_.loc.x <= num.loc.x + 1)
      if (candidateParts.exists(num.isAdjacent)) findPartNumsImpl(parts, nums.tail, num :: acc)
      else findPartNumsImpl(possibleParts, nums.tail, acc)
    }
  }

  def gearRatios(parts: List[Part], nums: List[Num]): Long = {
    val gears = parts.filter(_.part == '*')

    gearRatiosImpl(gears, nums)
  }


  @tailrec
  def gearRatiosImpl(gears: List[Part], nums: List[Num], acc: Long = 0): Long = {
    if (gears.isEmpty) acc
    else {
      val gear = gears.head

      val possibleNums = nums.dropWhile(_.loc.x < gear.loc.x - 1)
      val candidateNums = possibleNums.takeWhile(_.loc.x <= gear.loc.x + 1)

      val ratio = candidateNums.filter(_.isAdjacent(gear)) match {
        case a :: b :: Nil => a.num.toLong * b.num.toLong
        case _ => 0
      }

      gearRatiosImpl(gears.tail, possibleNums, acc + ratio)
    }
  }
}

case class Part(part: Char, loc: Point)

case class Num(num: String, loc: Point) {
  def isAdjacent(p: Part): Boolean =
    p.loc.x >= loc.x - 1 && p.loc.x <= loc.x + 1 && p.loc.y >= loc.y - 1 && p.loc.y <= loc.y + num.length
}
