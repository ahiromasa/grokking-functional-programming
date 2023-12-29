package chapter12

import cats.effect.*
import cats.implicits.*
import chapter11.TravelGuide.AttractionOrdering.*
import chapter11.TravelGuide.*
import chapter11.TravelGuide.model.PopCultureSubject.*
import chapter11.TravelGuide.model.*
import org.apache.jena.rdfconnection.*

object TravelGuide {
  def guideScore(guide: Guide): Int = {
    val descriptionScore = guide.attraction.description.map { _ => 30 }.getOrElse(0)
    val quantityScore = Math.min(40, guide.subjects.size * 10)
    val totalFollowers = guide.subjects.map {
      _ match
        case Artist(_, followers) => followers.toLong
        case _                    => 0
    }.sum
    val totalBoxOffice = guide.subjects.map {
      _ match
        case Movie(_, boxOffice) => boxOffice.toLong
        case _                   => 0
    }.sum
    val followersScore = Math.min(15, totalFollowers / 100_000).toInt
    val boxOfficeScore = Math.min(15, totalBoxOffice / 10_000_000).toInt
    descriptionScore + quantityScore + followersScore + boxOfficeScore
  }

  def connectionResource(address: String, endpoint: String): Resource[IO, RDFConnection] = {
    Resource.make(
      IO.blocking {
        RDFConnectionRemote
          .create()
          .destination(address)
          .queryEndpoint(endpoint)
          .build()
      }
    ) { connection => IO.blocking { connection.close() } }
  }

  case class SearchReport(badGuides: List[Guide], problems: List[String])

  def travelGuide(dataAccess: DataAccess, attractionName: String): IO[Either[SearchReport, Guide]] = {
    dataAccess
      .findAttractions(attractionName, ByLocationPopulation, 3)
      .attempt
      .flatMap {
        _ match {
          case Left(exception) => IO.pure(Left(SearchReport(List.empty, List(exception.getMessage))))
          case Right(attractions) =>
            attractions
              .map { attraction => travelGuideForAttraction(dataAccess, attraction) }
              .map { _.attempt }
              .parSequence
              .map { findGoodGuide }
        }
      }
  }

  def travelGuideForAttraction(dataAccess: DataAccess, attraction: Attraction): IO[Guide] = {
    List(
      dataAccess.findArtistsFromLocation(attraction.location.id, 2),
      dataAccess.findMoviesFromLocation(attraction.location.id, 2)
    ).parSequence
      .map { _.flatten }
      .map { subjects => Guide(attraction, subjects) }
  }

  def findGoodGuide(errorsOrGuides: List[Either[Throwable, Guide]]): Either[SearchReport, Guide] = {
    val guides = errorsOrGuides.collect { case Right(travelGuide) => travelGuide }
    val errors = errorsOrGuides.collect { case Left(exception) => exception.getMessage }
    guides.sortBy { guideScore }.reverse.headOption match {
      case Some(bestGuide) =>
        if guideScore(bestGuide) > 55 then Right(bestGuide) else Left(SearchReport(guides, errors))
      case None => Left(SearchReport(List.empty, errors))
    }
  }
}
