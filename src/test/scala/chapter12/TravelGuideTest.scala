package chapter12

import cats.effect.*
import cats.effect.unsafe.implicits.global
import chapter11.TravelGuide.AttractionOrdering.*
import chapter11.TravelGuide.*
import chapter11.TravelGuide.model.PopCultureSubject.*
import chapter11.TravelGuide.model.*
import chapter11.WikidataDataAccess.*
import chapter12.TravelGuide.SearchReport
import chapter12.TravelGuide.connectionResource
import chapter12.TravelGuide.guideScore
import chapter12.TravelGuide.{travelGuide as travelGuideOrSearchReport}
import munit.ScalaCheckSuite
import org.apache.jena.fuseki.main.FusekiServer
import org.apache.jena.query.DatasetFactory
import org.apache.jena.rdfconnection.RDFConnection
import org.apache.jena.riot.RDFDataMgr
import org.scalacheck.Prop.*
import org.scalacheck.*

import java.lang.Exception

class TravelGuideTest extends ScalaCheckSuite {
  test("score of a guide with a description, 0 artists, and 2 popular movies should be 65") {
    val guide = Guide(
      Attraction(
        "Yellowstone National Park",
        Some("first national park in the world"),
        Location(LocationId("Q1214"), "Wyoming", 586107)
      ),
      List(Movie("The Hateful Eight", 155760117), Movie("Heaven's Gate", 3484331))
    )

    assertEquals(guideScore(guide), 65)
  }

  test("score of a guide with no description, 0 artists, and 0 movies should be 0") {
    val guide = Guide(
      Attraction(
        "Yellowstone National Park",
        None,
        Location(LocationId("Q1214"), "Wyoming", 586107)
      ),
      List.empty
    )

    assertEquals(guideScore(guide), 0)
  }

  test("score of a guide with no description, 0 artists, and 2 movies with no box office earnings should be 20") {
    val guide = Guide(
      Attraction(
        "Yellowstone National Park",
        None,
        Location(LocationId("Q1214"), "Wyoming", 586107)
      ),
      List(Movie("The Hateful Eight", 0), Movie("Heaven's Gate", 0))
    )

    assertEquals(guideScore(guide), 20)
  }

  test("guide score should not depend on its attraction's name and description strings") {
    forAll { (name: String, description: String) =>
      {
        val guide = Guide(
          Attraction(
            name,
            Some(description),
            Location(LocationId("Q1214"), "Wyoming", 586107)
          ),
          List(Movie("The Hateful Eight", 155760117), Movie("Heaven's Gate", 3484331))
        )

        assertEquals(guideScore(guide), 65)
      }
    }
  }

  test("guide score should always be between 30 and 70 if it has a description and some bad movies") {
    forAll { (amountOfMovies: Byte) =>
      {
        val guide = Guide(
          Attraction(
            "Yellowstone National Park",
            Some("first national park in the world"),
            Location(LocationId("Q1214"), "Wyoming", 586107)
          ),
          if amountOfMovies > 0
          then List.fill(amountOfMovies) { Movie("Random Movie", 0) }
          else List.empty
        )

        val score = guideScore(guide)

        assert { score >= 30 && score <= 70 }
      }
    }
  }

  val nonNegativeInt: Gen[Int] = Gen.chooseNum(0, Int.MaxValue)

  test("guide score should always be between 20 and 50 if there is an artist and a movie but no description") {
    forAll(nonNegativeInt, nonNegativeInt) { (followers: Int, boxOffice: Int) =>
      {
        val guide = Guide(
          Attraction(
            "Yellowstone National Park",
            None,
            Location(LocationId("Q1214"), "Wyoming", 586107)
          ),
          List(Artist("Chris LeDoux", followers), Movie("The Hateful Eight", boxOffice))
        )

        val score = guideScore(guide)

        assert { score >= 20 && score <= 50 }
      }
    }
  }

  val randomArtist: Gen[Artist] = for {
    name <- Gen.identifier
    followers <- nonNegativeInt
  } yield Artist(name, followers)

  test("guide score should always be between 10 and 25 if there is just a single artist") {
    forAll(randomArtist) { (artist: Artist) =>
      {
        val guide = Guide(
          Attraction(
            "Yellowstone National Park",
            None,
            Location(LocationId("Q1214"), "Wyoming", 586107)
          ),
          List(artist)
        )

        val score = guideScore(guide)

        assert { score >= 10 && score <= 25 }
      }
    }
  }

  val randomArtists: Gen[List[Artist]] = for {
    numberOfArtists <- Gen.chooseNum(0, 100)
    artists <- Gen.listOfN(numberOfArtists, randomArtist)
  } yield artists

  test("guide score should always be between 0 and 55 if there is no description and no movies") {
    forAll(randomArtists) { (artists: List[Artist]) =>
      {
        val guide = Guide(
          Attraction(
            "Yellowstone National Park",
            None,
            Location(LocationId("Q1214"), "Wyoming", 586107)
          ),
          artists
        )

        val score = guideScore(guide)

        assert { score >= 0 && score <= 55 }
      }
    }
  }

  val randomMovie: Gen[Movie] = for {
    name <- Gen.identifier
    boxOffice <- nonNegativeInt
  } yield Movie(name, boxOffice)

  val randomMovies: Gen[List[Movie]] = for {
    numberOfMovies <- Gen.chooseNum(0, 100)
    movies <- Gen.listOfN(numberOfMovies, randomMovie)
  } yield movies

  val randomPopCultureSubjects: Gen[List[PopCultureSubject]] = for {
    movies <- randomMovies
    artists <- randomArtists
  } yield movies.appendedAll(artists)

  test("guide score should always be between 0 and 70 if it only contains pop culture subjects") {
    forAll(randomPopCultureSubjects) { (popCultureSubjects: List[PopCultureSubject]) =>
      {
        val guide = Guide(
          Attraction(
            "Yellowstone National Park",
            None,
            Location(LocationId("Q1214"), "Wyoming", 586107)
          ),
          popCultureSubjects
        )

        val score = guideScore(guide)

        assert { score >= 0 && score <= 70 }
      }
    }
  }

  test("travel guide should include artists originating from the attraction's location") {
    val attractionName = "Tower Bridge"
    val london = Location(LocationId("Q84"), "London", 8_908_081)
    val queen: Artist = Artist("Queen", 2_050_559)

    val dataAccess = new DataAccess {
      def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]] = {
        IO.pure(List(Attraction(attractionName, None, london)))
      }

      def findArtistsFromLocation(locationId: LocationId, limit: Int): IO[List[Artist]] = {
        if locationId == london.id then IO.pure(List(queen)) else IO.pure(List.empty)
      }

      def findMoviesFromLocation(locationId: LocationId, limit: Int): IO[List[Movie]] = {
        IO.pure(List.empty)
      }
    }

    val guide = travelGuide(dataAccess, attractionName).unsafeRunSync()

    assert { guide.exists { _.subjects == List(queen) } }
  }

  test("travel guide should include movies set in the attraction's location") {
    val attractionName = "Golden Gate Bridge"
    val sanFrancisco = Location(LocationId("Q62"), "San Francisco", 883_963)
    val insideOut: Movie = Movie("Inside Out", 857_611_174)

    val dataAccess = new DataAccess {
      def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]] = {
        IO.pure(List(Attraction(attractionName, None, sanFrancisco)))
      }

      def findArtistsFromLocation(locationId: LocationId, limit: Int): IO[List[Artist]] = {
        IO.pure(List.empty)
      }

      def findMoviesFromLocation(locationId: LocationId, limit: Int): IO[List[Movie]] = {
        if locationId == sanFrancisco.id then IO.pure(List(insideOut)) else IO.pure(List.empty)
      }
    }

    val guide = travelGuide(dataAccess, attractionName).unsafeRunSync()

    assert { guide.exists { _.subjects == List(insideOut) } }
  }

  def localSparqlServer: Resource[IO, FusekiServer] = {
    val start = IO.blocking {
      val model = RDFDataMgr.loadModel(getClass().getResource("/testdata.ttl").toString())
      val ds = DatasetFactory.create(model)
      val server = FusekiServer.create().add("/test", ds).build()
      server.start()
      server
    }

    Resource.make(start) { server => IO.blocking { server.stop() } }
  }

  val testServerConnection: Resource[IO, RDFConnection] = for {
    localServer <- localSparqlServer
    connection <- connectionResource(localServer.serverURL(), "test")
  } yield connection

  test("data access layer should fetch attractions from a real SPARQL server") {
    val result = testServerConnection
      .use { connection =>
        {
          val dataAccess = getSparqlDataAccess(execQuery(connection))
          dataAccess.findAttractions("Bridge of Sighs", ByLocationPopulation, 5)
        }
      }
      .unsafeRunSync()

    assert { result.exists { _.name == "Bridge of Sighs" } && result.size <= 5 }
  }

  test("data access layer should fetch attractions sorted by location population") {
    val locations = testServerConnection
      .use { connection =>
        {
          val dataAccess = getSparqlDataAccess(execQuery(connection))
          dataAccess.findAttractions("Yellowstone", ByLocationPopulation, 3)
        }
      }
      .unsafeRunSync()
      .map { _.location }

    assert { locations.size == 3 && locations == locations.sortBy { _.population }.reverse }
  }

  test("data access layer should fetch attractions sorted by name") {
    val attractions = testServerConnection
      .use { connection =>
        {
          val dataAccess = getSparqlDataAccess(execQuery(connection))
          dataAccess.findAttractions("National Park", ByName, 5)
        }
      }
      .unsafeRunSync()

    assert { attractions.size == 5 && attractions.map { _.name } == attractions.sortBy { _.name }.map { _.name } }
  }

  val veniceId: LocationId = LocationId("Q641")

  test("data access layer should fetch artists from a real SPARQL server") {
    val artists = testServerConnection
      .use { connection =>
        {
          val dataAccess = getSparqlDataAccess(execQuery(connection))
          dataAccess.findArtistsFromLocation(veniceId, 1)
        }
      }
      .unsafeRunSync()

    assertEquals(artists.map { _.name }, List("Talco"))
  }

  test("data access layer should fetch movies from a real SPARQL server") {
    val movies = testServerConnection
      .use { connection =>
        {
          val dataAccess = getSparqlDataAccess(execQuery(connection))
          dataAccess.findMoviesFromLocation(veniceId, 2)
        }
      }
      .unsafeRunSync()

    assertEquals(movies.map { _.name }, List("Spider-Man: Far from Home", "Casino Royale"))
  }

  test("data access layer should accept and relay limit values to a real SPARQL server") {
    forAll(nonNegativeInt) { (limit: Int) =>
      {
        val movies = testServerConnection
          .use { connection =>
            {
              val dataAccess = getSparqlDataAccess(execQuery(connection))
              dataAccess.findMoviesFromLocation(veniceId, limit)
            }
          }
          .unsafeRunSync()

        assert { movies.size <= limit }
      }
    }
  }

  def dataAccessStub(
    attractions: IO[List[Attraction]],
    artists: IO[List[Artist]],
    movies: IO[List[Movie]]
  ): DataAccess = {
    new DataAccess {
      def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]] = {
        attractions
      }

      def findArtistsFromLocation(locationId: LocationId, limit: Int): IO[List[Artist]] = {
        artists
      }

      def findMoviesFromLocation(locationId: LocationId, limit: Int): IO[List[Movie]] = {
        movies
      }
    }
  }

  val yellowstone: Attraction = Attraction(
    "Yellowstone National Park",
    Some("first national park in the world"),
    Location(LocationId("Q1214"), "Wyoming", 586107)
  )

  test("travelGuide should return a search report if it can't find a good-enough guide") {
    val dataAccess = dataAccessStub(
      IO.pure(List(yellowstone)),
      IO.pure(List.empty),
      IO.pure(List.empty)
    )

    val result = travelGuideOrSearchReport(dataAccess, "").unsafeRunSync()

    assertEquals(result, Left(SearchReport(List(Guide(yellowstone, List.empty)), List.empty)))
  }

  val hatefulEight: Movie = Movie("The Hateful Eight", 155760117)
  val heavensGate: Movie = Movie("Heaven's Gate", 3484331)

  test("travelGuide should return a travel guide if two movies are available") {
    val dataAccess = dataAccessStub(
      IO.pure(List(yellowstone)),
      IO.pure(List.empty),
      IO.pure(List(hatefulEight, heavensGate))
    )

    val result = travelGuideOrSearchReport(dataAccess, "").unsafeRunSync()

    assertEquals(result, Right(Guide(yellowstone, List(hatefulEight, heavensGate))))
  }

  test("travelGuide should return a search report with problems when fetching attractions fails") {
    val dataAccess = dataAccessStub(
      IO.delay { throw new Exception("fetching failed") },
      IO.pure(List.empty),
      IO.pure(List.empty)
    )

    val result = travelGuideOrSearchReport(dataAccess, "").unsafeRunSync()

    assertEquals(result, Left(SearchReport(List.empty, List("fetching failed"))))
  }

  val yosemite: Attraction = Attraction(
    "Yosemite National Park",
    Some("national park in California, United States"),
    Location(LocationId("Q109661"), "Madera County", 157327)
  )

  test("travelGuide should return a search report with some guides if it can't fetch artists due to IO failures") {
    val dataAccess =
      new DataAccess {
        def findAttractions(name: String, ordering: AttractionOrdering, limit: Int) = {
          IO.pure(List(yosemite, yellowstone))
        }

        def findArtistsFromLocation(locationId: LocationId, limit: Int) = {
          if locationId == yosemite.location.id
          then IO.delay { throw new Exception("Yosemite artists fetching failed") }
          else IO.pure(List.empty)
        }

        def findMoviesFromLocation(locationId: LocationId, limit: Int) = {
          IO.pure(List.empty)
        }
      }

    val result = travelGuideOrSearchReport(dataAccess, "").unsafeRunSync()

    assertEquals(
      result,
      Left(SearchReport(List(Guide(yellowstone, List.empty)), List("Yosemite artists fetching failed")))
    )
  }
}
