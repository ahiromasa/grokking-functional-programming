package chapter06

object TvShows {
  case class TvShow(title: String, start: Int, end: Int)

  def sortShows(shows: List[TvShow]): List[TvShow] = {
    shows.sortBy { show => show.end - show.start }.reverse
  }

  def parseShows(rawShows: List[String]): Either[String, List[TvShow]] = {
    rawShows.map { parseShow }.foldLeft(Right(List.empty)) { addOrResign }
  }

//   def sortRawShows(rawShows: List[String]): List[TvShow] = {
//     val shows = parseShows(rawShows)
//     sortShows(shows)
//   }

  def parseShow(rawShow: String): Either[String, TvShow] = {
    for {
      name <- extractName(rawShow)
      yearStart <- extractYearStart(rawShow).orElse(extractSingleYear(rawShow))
      yearEnd <- extractYearEnd(rawShow).orElse(extractSingleYear(rawShow))
    } yield TvShow(name, yearStart, yearEnd)
  }

  def extractName(rawShow: String): Either[String, String] = {
    val bracketOpen = rawShow.indexOf('(')
    if bracketOpen != -1 then Right(rawShow.substring(0, bracketOpen).trim())
    else Left(s"Can't extract name from ${rawShow}")
  }

  def extractYearStart(rawShow: String): Either[String, Int] = {
    val bracketOpen = rawShow.indexOf('(')
    val dash = rawShow.indexOf('-')
    for {
      year <-
        if bracketOpen != -1 && dash > bracketOpen + 1 then Right(rawShow.substring(bracketOpen + 1, dash))
        else Left(s"Can't extract start year from ${rawShow}")
      year <- year.toIntOption.toRight(s"Can't parse ${year}")
    } yield year
  }

  def extractYearEnd(rawShow: String): Either[String, Int] = {
    val dash = rawShow.indexOf('-')
    val bracketClose = rawShow.indexOf(')')
    for {
      year <-
        if dash != -1 && bracketClose > dash + 1 then Right(rawShow.substring(dash + 1, bracketClose))
        else Left(s"Can't extract end year from ${rawShow}")
      year <- year.toIntOption.toRight(s"Can't parse ${year}")
    } yield year
  }

  def extractSingleYear(rawShow: String): Either[String, Int] = {
    val bracketOpen = rawShow.indexOf('(')
    val dash = rawShow.indexOf('-')
    val bracketClose = rawShow.indexOf(')')
    for {
      year <-
        if dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1 then
          Right(rawShow.substring(bracketOpen + 1, bracketClose))
        else Left(s"Can't extract single year from ${rawShow}")
      year <- year.toIntOption.toRight(s"Can't parse ${year}")
    } yield year
  }

//   def extractSingleYearOrYearEnd(rawShow: String): Option[Int] = {
//     extractSingleYear(rawShow).orElse(extractYearEnd(rawShow))
//   }

//   def extractAnyYear(rawShow: String): Option[Int] = {
//     extractYearStart(rawShow).orElse(extractYearEnd(rawShow)).orElse(extractSingleYear(rawShow))
//   }

//   def extractSingleYearIfNameExists(rawShow: String): Option[Int] = {
//     extractName(rawShow).flatMap { _ => extractSingleYear(rawShow) }
//   }

//   def extractAnyYearIfNameExists(rawShow: String): Option[Int] = {
//     extractName(rawShow).flatMap { _ => extractAnyYear(rawShow) }
//   }

//   def addOrResign(parsedShows: Option[List[TvShow]], newParsedShow: Option[TvShow]): Option[List[TvShow]] = {
//     for {
//       shows <- parsedShows
//       show <- newParsedShow
//     } yield shows.appended(show)
//   }

//   def parseShowsOption(rawShows: List[String]): Option[List[TvShow]] = {
//     rawShows.map { parseShow }.foldLeft(Some(List.empty)) { addOrResign }
//   }

  def addOrResign(
      parsedShows: Either[String, List[TvShow]],
      newParsedShow: Either[String, TvShow]
  ): Either[String, List[TvShow]] = {
    for {
      shows <- parsedShows
      show <- newParsedShow
    } yield shows.appended(show)
  }
}
