package aoc2023

import cats.effect.IO
import scala.annotation.tailrec

object Lottery {
  def points(tickets: fs2.Stream[IO, String]): IO[String] =
    tickets.evalMap(Ticket.parse)
      .map(_.pointValue)
      .compile
      .toList
      .map(_.sum.toString)

  def totalTickets(tickets: fs2.Stream[IO, String]): IO[String] =
    tickets.evalMap(Ticket.parse)
      .compile
      .toList
      .map(tl => processTickets(tl).toString)

  def processTickets(tickets: List[Ticket]): Long = {
    @tailrec
    def processTicketsImpl(remaining: List[Ticket], totals: TicketTotals): Long = {
      if (remaining.isEmpty) totals.total
      else {
        val ticket = remaining.head
        val rest = remaining.tail
        val count = totals.counts.getOrElse(ticket.id, 0L)
        val updatedTotals = totals.include(rest.take(ticket.winnerCount).map(_.id), count)
        processTicketsImpl(rest, updatedTotals)
      }
    }

    processTicketsImpl(tickets, TicketTotals.init(tickets))
  }
}

case class Ticket(id: Int, winningNumbers: Set[Int], playerNumbers: Set[Int]) {
  def pointValue: Long = {
    val winners = winningNumbers & playerNumbers

    if (winners.nonEmpty) math.pow(2, winners.size.toDouble - 1).toLong
    else 0
  }

  def winnerCount: Int = (winningNumbers & playerNumbers).size
}

object Ticket {
  def parse(s: String): IO[Ticket] = s match {
    case s"Card $n: $wn | $pn" =>
      val id = n.trim.toInt
      val winningNumbers = wn.split("\\s+").filterNot(_ == "").map(_.trim.toInt).toSet
      val playerNumbers = pn.split("\\s+").filterNot(_ == "").map(_.trim.toInt).toSet
      IO.pure(Ticket(id, winningNumbers, playerNumbers))
    case _ =>
      IO.raiseError(new RuntimeException(s"Unable to parse ticket: $s"))
  }
}

case class TicketTotals(counts: Map[Int, Long]) {
  def total: Long = counts.values.sum

  def include(tickets: List[Int], count: Long): TicketTotals = {
    @tailrec
    def includeImpl(t: List[Int], m: Map[Int, Long] = counts): Map[Int, Long] = {
      if (t.isEmpty) m
      else includeImpl(t.tail, m + (t.head -> (m.getOrElse(t.head, 0L) + count)))
    }

    TicketTotals(includeImpl(tickets))
  }
}

object TicketTotals {
  def init(tickets: List[Ticket]): TicketTotals = TicketTotals(tickets.map(_.id -> 1L).toMap)
}
