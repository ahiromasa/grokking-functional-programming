package chapter07

object MusicArtistsSearch {
  object model {
    opaque type Location = String

    object Location {
      def apply(value: String): Location = value

      extension (self: Location) {
        def value: String = self
      }
    }

    opaque type Genre = String

    object Genre {
      def apply(value: String): Genre = value

      extension (self: Genre) {
        def value: String = self
      }
    }

    opaque type YearsActiveStart = Int

    object YearsActiveStart {
      def apply(value: Int): YearsActiveStart = value

      extension (self: YearsActiveStart) {
        def value: Int = self
      }
    }

    opaque type YearsActiveEnd = Int

    object YearsActiveEnd {
      def apply(value: Int): YearsActiveEnd = value

      extension (self: YearsActiveEnd) {
        def value: Int = self
      }
    }

    case class PeriodInYears(start: Int, end: Int)

    enum MusicGenre {
      case HeavyMetal
      case Pop
      case HardRock
    }

    enum YearsActive {
      case StillActive(since: Int, previousPeriods: List[PeriodInYears])
      case ActiveInPast(periods: List[PeriodInYears])
    }

    enum SearchCondition {
      case SearchByGenre(genres: List[MusicGenre])
      case SearchByOrigin(locations: List[Location])
      case SearchByActiveYears(period: PeriodInYears)
      case SearchByActiveLength(howLong: Int, until: Int)
    }

    case class Artist(
      name: String,
      genre: MusicGenre,
      origin: Location,
      yearsActive: YearsActive
    )
  }

  import model.*
  import model.YearsActive.*

  def searchArtists(artists: List[Artist], requiredConditions: List[SearchCondition]): List[Artist] =
    artists.filter { artist =>
      requiredConditions.forall {
        _ match {
          case SearchCondition.SearchByGenre(genres)                => genres.contains(artist.genre)
          case SearchCondition.SearchByOrigin(locations)            => locations.contains(artist.origin)
          case SearchCondition.SearchByActiveYears(period)          => wasArtistActive(artist, period)
          case SearchCondition.SearchByActiveLength(howLong, until) => activeLength(artist, until) >= howLong
        }
      }
    }

  def wasArtistActive(artist: Artist, searchedPeriod: PeriodInYears): Boolean =
    artist.yearsActive match {
      case StillActive(since, previousPeriods) =>
        since <= searchedPeriod.end || periodOverlapsWithPeriods(searchedPeriod, previousPeriods)
      case ActiveInPast(periods) =>
        periodOverlapsWithPeriods(searchedPeriod, periods)
    }

  def activeLength(artist: Artist, currentYear: Int): Int = {
    val periods = artist.yearsActive match {
      case StillActive(since, previousPeriods) => previousPeriods.appended(PeriodInYears(since, currentYear))
      case ActiveInPast(periods)               => periods
    }
    periods.map { period => period.end - period.start }.foldLeft(0) { _ + _ }
  }

  def periodOverlapsWithPeriods(checkedPeriod: PeriodInYears, periods: List[PeriodInYears]): Boolean =
    periods.exists { period =>
      period.start <= checkedPeriod.end && period.end >= checkedPeriod.start
    }
}
