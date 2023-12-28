package chapter11

import cats.effect.IO
import cats.implicits.*
import chapter11.TravelGuide.AttractionOrdering
import chapter11.TravelGuide.AttractionOrdering.*
import chapter11.TravelGuide.*
import chapter11.TravelGuide.model.PopCultureSubject.*
import chapter11.TravelGuide.model.*
import org.apache.jena.query.QuerySolution

object WikidataDataAccess {
  def getSparqlDataAccess(execQuery: String => IO[List[QuerySolution]]): DataAccess = {
    val prefix = """
      |PREFIX wd: <http://www.wikidata.org/entity/>
      |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX schema: <http://schema.org/>
      |""".stripMargin

    new DataAccess {
      def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]] = {
        val orderBy = ordering match {
          case ByName               => "?attractionLabel"
          case ByLocationPopulation => "DESC(?population)"
        }

        val query = s"""
          |${prefix}
          |SELECT DISTINCT ?attraction ?attractionLabel ?description ?location ?locationLabel ?population WHERE {
          |  ?attraction wdt:P31 wd:Q570116;
          |    rdfs:label ?attractionLabel;
          |    wdt:P131 ?location.
          |  FILTER((LANG(?attractionLabel)) = "en")
          |  OPTIONAL {
          |    ?attraction schema:description ?description.
          |    FILTER((LANG(?description)) = "en")
          |  }
          |  ?location wdt:P1082 ?population;
          |    rdfs:label ?locationLabel.
          |  FILTER((LANG(?locationLabel)) = "en")
          |  FILTER(CONTAINS(?attractionLabel, "${name}"))
          |} ORDER BY ${orderBy} LIMIT ${limit}
          |""".stripMargin

        for {
          solutions <- execQuery(query)
          attractions <- solutions.traverse(parseAttractions)
        } yield attractions
      }

      def findArtistsFromLocation(locationId: LocationId, limit: Int): IO[List[Artist]] = {
        val query = s"""
        |${prefix}
        |SELECT DISTINCT ?artist ?artistLabel ?followers WHERE {
        |  ?artist wdt:P136 ?genre;
        |    wdt:P8687 ?followers;
        |    rdfs:label ?artistLabel.
        |  FILTER(LANG(?artistLabel) = "en").
        |  ?artist wdt:P740 wd:${locationId.value}
        |} ORDER BY DESC(?followers) LIMIT ${limit}
        |""".stripMargin

        for {
          solutions <- execQuery(query)
          artists <-
            IO.delay {
              solutions.map[Artist] { solution =>
                Artist(
                  name = solution.getLiteral("artistLabel").getString,
                  followers = solution.getLiteral("followers").getInt
                )
              }
            }
        } yield artists
      }

      def findMoviesFromLocation(locationId: LocationId, limit: Int): IO[List[Movie]] = {
        val query = s"""
          |${prefix}
          |SELECT DISTINCT ?subject ?subjectLabel ?boxOffice WHERE {
          |  ?subject wdt:P31 wd:Q11424;
          |    wdt:P2142 ?boxOffice;
          |    rdfs:label ?subjectLabel.
          |  ?subject wdt:P840 wd:${locationId.value}
          |  FILTER(LANG(?subjectLabel) = "en").
          |} ORDER BY DESC(?boxOffice) LIMIT ${limit}
          |""".stripMargin

        for {
          solutions <- execQuery(query)
          movies <- IO.delay {
            solutions.map[Movie] { solution =>
              Movie(
                name = solution.getLiteral("subjectLabel").getString,
                boxOffice = solution.getLiteral("boxOffice").getInt
              )
            }
          }
        } yield movies
      }
    }
  }

  def parseAttractions(solution: QuerySolution): IO[Attraction] = {
    IO.delay {
      Attraction(
        name = solution.getLiteral("attractionLabel").getString(),
        description =
          if solution.contains("description")
          then Some(solution.getLiteral("description").getString())
          else None,
        location = Location(
          id = LocationId(solution.getResource("location").getLocalName()),
          name = solution.getLiteral("locationLabel").getString(),
          population = solution.getLiteral("population").getInt()
        )
      )
    }
  }
}
