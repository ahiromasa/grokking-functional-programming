package chapter11

import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import chapter11.WikidataDataAccess.*
import org.apache.jena.query.QueryExecution
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.QuerySolution
import org.apache.jena.rdfconnection.RDFConnection
import org.apache.jena.rdfconnection.RDFConnectionRemote

import scala.jdk.javaapi.CollectionConverters.asScala

object TravelGuide {
  object model {
    opaque type LocationId = String

    object LocationId {
      def apply(value: String): LocationId = value

      extension (self: LocationId) {
        def value: String = self
      }
    }

    case class Location(id: LocationId, name: String, population: Int)

    case class Attraction(name: String, description: Option[String], location: Location)

    enum PopCultureSubject {
      case Artist(name: String, followers: Int)
      case Movie(name: String, boxOffice: Int)
    }

    case class Guide(attraction: Attraction, subjects: List[PopCultureSubject])
  }

  import model.*
  import model.PopCultureSubject.*

  enum AttractionOrdering {
    case ByName
    case ByLocationPopulation
  }

  import AttractionOrdering.*

  trait DataAccess {
    def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]]
    def findArtistsFromLocation(locationId: LocationId, limit: Int): IO[List[Artist]]
    def findMoviesFromLocation(locationId: LocationId, limit: Int): IO[List[Movie]]
  }

  def travelGuide(data: DataAccess, attractionName: String): IO[Option[Guide]] = {
    for {
      attractions <- data.findAttractions(attractionName, ByLocationPopulation, 3)
      guides <- attractions.map { attraction =>
        List(
          data.findArtistsFromLocation(attraction.location.id, 2),
          data.findMoviesFromLocation(attraction.location.id, 2)
        ).parSequence
          .map { _.flatten }
          .map { Guide(attraction, _) }
      }.parSequence
    } yield guides.sortBy(guideScore).reverse.headOption
  }

  def execQuery(connection: RDFConnection)(query: String): IO[List[QuerySolution]] = {
    val executionResource: Resource[IO, QueryExecution] =
      Resource.make(createExecution(connection, query))(closeExecution)
    executionResource.use { execution => IO.blocking { asScala(execution.execSelect()).toList } }
  }

  def guideScore(guide: Guide): Int = {
    val descriptionScore = guide.attraction.description.map { _ => 30 }.getOrElse(0)
    val quantityScore = Math.min(40, guide.subjects.size * 10)
    val totalFollowers = guide.subjects.map {
      _ match
        case Artist(_, followers) => followers
        case _                    => 0
    }.sum
    val totalBoxOffice = guide.subjects.map {
      _ match
        case Movie(_, boxOffice) => boxOffice
        case _                   => 0
    }.sum
    val followersScore = Math.min(15, totalFollowers / 100_000)
    val boxOfficeScore = Math.min(15, totalBoxOffice / 10_000_000)
    descriptionScore + quantityScore + followersScore + boxOfficeScore
  }

  def createExecution(connection: RDFConnection, query: String): IO[QueryExecution] = {
    IO.blocking { connection.query(QueryFactory.create(query)) }
  }

  def closeExecution(execution: QueryExecution): IO[Unit] = {
    IO.blocking { execution.close() }
  }

  val connectionResource: Resource[IO, RDFConnection] = Resource.make(IO.blocking {
    RDFConnectionRemote
      .create()
      .destination("https://query.wikidata.org/")
      .queryEndpoint("sparql")
      .build()
  }) { connection => IO.blocking { connection.close() } }

  val dataAccessResource: Resource[IO, DataAccess] = connectionResource.map { connection =>
    getSparqlDataAccess(execQuery(connection))
  }

  @main def run = {
    dataAccessResource
      .use { travelGuide(_, "Yellowstone") >>= IO.println }
      .unsafeRunSync()
  }

  def cachedExecQuery(connection: RDFConnection, cache: Ref[IO, Map[String, List[QuerySolution]]])(
      query: String
  ): IO[List[QuerySolution]] = {
    for {
      cachedQueries <- cache.get
      solutions <- cachedQueries.get(query) match {
        case Some(cachedSolutions) => IO.pure(cachedSolutions)
        case None =>
          for {
            realSolutions <- execQuery(connection)(query)
            _ <- cache.update { _.updated(query, realSolutions) }
          } yield realSolutions
      }
    } yield solutions
  }
}
