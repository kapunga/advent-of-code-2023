package aoc2023

import cats.effect.IO

object BoatGame {
  def optionsMulti(input: fs2.Stream[IO, String]): IO[String] = {
    for {
      inputList <- input.compile.toList
      races <- Race.toRaces(inputList)
    } yield races.map(_.betterOptions).product.toString
  }

  def optionsSingle(input: fs2.Stream[IO, String]): IO[String] = {
    for {
      inputList <- input.compile.toList
      races <- Race.toRaces(inputList, true)
    } yield races.map(_.betterOptions).product.toString
  }
 }

case class Race(time: Long, record: Long) {
  def betterOptions: Long = {
    val low = (-time + math.sqrt((time * time) - (4 * record))) / -2
    val high = (-time - math.sqrt((time * time) - (4 * record))) / -2

    math.abs(math.floor(low + 1).toLong - math.ceil(high).toLong)
  }
}

object Race {
  def toRaces(input: List[String], single: Boolean = false): IO[List[Race]] = input match {
    case t :: r :: Nil =>
      for {
        times <- parseString(t, single)
        records <- parseString(r, single)
        _ <- IO.raiseWhen(times.length != records.length)(
          new Exception(s"Mismatched lengths: $times - $records"))
      } yield times.zip(records).map((t, r) => Race(t, r))
    case _ => IO.raiseError(new Exception(s"Too many input rows: ${input.length}"))
  }

  private def parseString(s: String, stripSpace: Boolean): IO[List[Long]] = {
    def toLongList(ss: String): IO[List[Long]] =
      if (stripSpace) IO(ss.replaceAll("\\s+", "").toLong :: Nil)
      else IO(ss.trim.split("\\s+").map(_.toLong).toList)

    s match {
      case s"Time: $numbers" => toLongList(numbers)
      case s"Distance: $numbers" => toLongList(numbers)
    }
  }
}


