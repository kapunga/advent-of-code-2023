package aoc2023

import cats.effect.IO
import scala.annotation.tailrec

object CalibrationConfig {
  val nums: Map[String, String] = Map(
    "zero" -> "0",
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9")
  
  def read(parseWords: Boolean)(input: fs2.Stream[IO, String]): IO[String] = {
    val parsedInput = if (parseWords) input.map(s => convertNumbers(s)) else input
      
    parsedInput
      .map(RowValue.fromString)
      .map(_.asLong)
      .compile
      .fold(0L)((a, n) => a + n)
      .map(_.toString)
  }

  @tailrec
  private def convertNumbers(str: String, out: String = ""): String = {
    if (str.isEmpty) out
    else {
      val maybeNumber: String =
        nums.keys.find(str.startsWith)
          .map(nums.apply)
          .getOrElse(str.head.toString)
      convertNumbers(str.tail, out + maybeNumber)
    }
  }
}

case class RowValue(fl: Option[(Char, Char)] = None) {
  def addChar(char: Char): RowValue = {
    if (char.isDigit) {
      fl match {
        case Some((f, _)) => RowValue(Some((f, char)))
        case None => RowValue(Some((char, char)))
      }
    } else this
  }

  def asLong: Long = fl.map({ case (a, b) => s"$a$b".toLong }).getOrElse(0)
}

object RowValue {
  def fromString(str: String): RowValue = {
    str.toCharArray.iterator.foldLeft(RowValue())({
      case (fl, c) => fl.addChar(c)
    })
  }
}
