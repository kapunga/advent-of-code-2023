package aoc2023

import cats.effect.IO
import cats.effect.std.Console

object CamelCards {
  def winnings(input: fs2.Stream[IO, String]): IO[String] =
    input
      .evalMap(Hand.parse)
      .compile
      .toList
      .map(_.sorted)
      .map(_.zipWithIndex
        .map((h, i) => h.bid * (i + 1))
        .sum
        .toString)

  def wildWinnings(input: fs2.Stream[IO, String]): IO[String] =
    input
      .evalMap(WildHand.parse)
      .compile
      .toList
      .map(_.sorted)
      .map(_.zipWithIndex
        .map((h, i) => h.bid * (i + 1))
        .sum
        .toString
      )
}

enum HandType {
  case HighCard, Pair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
}

case class Hand(cards: String, bid: Long) {
  def handType: HandType = {
    val counts = cards.toList.groupBy(c => c)

    counts.values.map(_.length).toList.sorted.reverse match {
      case 5 :: Nil => HandType.FiveOfAKind
      case 4 :: 1 :: Nil => HandType.FourOfAKind
      case 3 :: 2 :: Nil => HandType.FullHouse
      case 3 :: 1 :: _ => HandType.ThreeOfAKind
      case 2 :: 2 :: _ => HandType.TwoPair
      case 2 :: 1 :: _ => HandType.Pair
      case _ => HandType.HighCard
    }
  }

}

object Hand {
  private val cardStrength = "23456789TJQKA"

  given Ordering[Hand] with
    def compare(x: Hand, y: Hand): Int = {
      val xType = x.handType
      val yType = y.handType

      if (xType != yType) xType.ordinal - yType.ordinal
      else {
        val cardPairs = x.cards.toList.zip(y.cards.toList)
        cardPairs.find((a, b) => a != b)
          .map((a, b) => cardStrength.indexOf(a) - cardStrength.indexOf(b))
          .getOrElse(0)
      }
    }

  def parse(s: String): IO[Hand] = s match {
    case s"$cards $bid" =>
      if (cards.length != 5) IO.raiseError(new Exception(s"Invalid hand size: $cards"))
      else IO(Hand(cards, bid.toLong))
    case _ => IO.raiseError(new Exception(s"Invalid hand parameter: $s"))
  }
}

case class WildHand(cards: String, bid: Long) {
  def handType: HandType = {
    val counts = cards.toList.groupBy(c => c)

    val wildCards = counts.getOrElse('J', Nil).length
    val noJCounts = counts.removed('J')

    val countList =
      noJCounts.values.map(_.length).toList.sorted.reverse

    val augmented = {
      if (countList == Nil) wildCards :: Nil
      else (countList.head + wildCards) :: countList.tail
    }

    augmented match {
      case 5 :: Nil => HandType.FiveOfAKind
      case 4 :: 1 :: Nil => HandType.FourOfAKind
      case 3 :: 2 :: Nil => HandType.FullHouse
      case 3 :: 1 :: _ => HandType.ThreeOfAKind
      case 2 :: 2 :: _ => HandType.TwoPair
      case 2 :: 1 :: _ => HandType.Pair
      case _ => HandType.HighCard
    }
  }

}

object WildHand {
  private val cardStrength = "J23456789TQKA"

  given Ordering[WildHand] with
    def compare(x: WildHand, y: WildHand): Int = {
      val xType = x.handType
      val yType = y.handType

      if (xType != yType) xType.ordinal - yType.ordinal
      else {
        val cardPairs = x.cards.toList.zip(y.cards.toList)
        cardPairs.find(_ != _)
          .map((a, b) => cardStrength.indexOf(a) - cardStrength.indexOf(b))
          .getOrElse(0)
      }
    }

  def parse(s: String): IO[WildHand] = s match {
    case s"$cards $bid" =>
      if (cards.length != 5) IO.raiseError(new Exception(s"Invalid hand size: $cards"))
      else IO(WildHand(cards, bid.toLong))
    case _ => IO.raiseError(new Exception(s"Invalid hand parameter: $s"))
  }
}
