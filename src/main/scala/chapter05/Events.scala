package chapter05

object Events {
  case class Event(name: String, start: Int, end: Int)

  def validateName(name: String): Option[String] = {
    if name.size > 0 then Some(name)
    else None
  }

  def validateEnd(end: Int): Option[Int] = {
    if end < 3000 then Some(end)
    else None
  }

  def validateStart(start: Int, end: Int): Option[Int] = {
    if start <= end then Some(start)
    else None
  }

  def parse(name: String, start: Int, end: Int): Option[Event] = {
    for {
      validName <- validateName(name)
      validEnd <- validateEnd(end)
      validStart <- validateStart(start, end)
    } yield Event(validName, validStart, validEnd)
  }

  def validateLength(start: Int, end: Int, minLength: Int): Option[Int] = {
    if end - start >= minLength then Some(end - start)
    else None
  }

  def parseLongEvent(name: String, start: Int, end: Int, minLength: Int): Option[Event] = {
    for {
      validName <- validateName(name)
      validEnd <- validateEnd(end)
      validStart <- validateStart(start, end)
      _ <- validateLength(validStart, validEnd, minLength)
    } yield Event(validName, validStart, validEnd)
  }
}
