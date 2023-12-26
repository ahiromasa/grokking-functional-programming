package chapter10

import cats.effect.IO
import cats.effect.Ref
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import fs2.Stream

import concurrent.duration.*

object CheckIns {
  object model {
    opaque type City = String

    object City {
      def apply(value: String): City = value

      extension (self: City) {
        def value: String = self
      }
    }

    case class CityStats(city: City, checkIns: Int)
  }

  import model.*

  val checkIns: Stream[IO, City] =
    Stream(City("Sydney"), City("Dublin"), City("Cape Town"), City("Lima"), City("Singapore"))
      .repeatN(100_000)
      .append(Stream.range(0, 100_000).map { i => City(s"City ${i}") })
      .append(Stream(City("Sydney"), City("Sydney"), City("Lima")))
      .covary[IO]

  def topCities(cityCheckIns: Map[City, Int]): List[CityStats] = {
    cityCheckIns.toList
      .map { case (city, checkIns) => CityStats(city, checkIns) }
      .sortBy(_.checkIns)
      .reverse
      .take(3)
  }

  def processCheckIns(checkIns: Stream[IO, City]): IO[ProcessingCheckIns] = {
    for {
      storedCheckIns <- Ref.of[IO, Map[City, Int]](Map.empty)
      storedRanking <- Ref.of[IO, List[CityStats]](List.empty)
      rankingProgram = updateRanking(storedCheckIns, storedRanking)
      checkInsProgram = checkIns.evalMap { storeCheckIn(storedCheckIns) }.compile.drain
      outputProgram = IO.sleep(1.second).flatMap { _ => storedRanking.get }.flatMap { IO.println }.foreverM
      fiber <- List(rankingProgram, checkInsProgram, outputProgram).parSequence.start
    } yield ProcessingCheckIns(storedRanking.get, fiber.cancel)
  }

  val example: IO[Int] = for {
    counter <- Ref.of[IO, Int](0)
    _ <- counter.update { _ + 3 }
    result <- counter.get
  } yield result

  assert { example.unsafeRunSync() == 3 }

  val exampleSequential: IO[Int] = for {
    counter <- Ref.of[IO, Int](0)
    _ <- List(
      counter.update { _ + 2 },
      counter.update { _ + 3 },
      counter.update { _ + 4 }
    ).sequence
    result <- counter.get
  } yield result

  assert { exampleSequential.unsafeRunSync() == 9 }

  def updateRanking(storedCheckIns: Ref[IO, Map[City, Int]], storedRanking: Ref[IO, List[CityStats]]): IO[Nothing] = {
    (for {
      newRanking <- storedCheckIns.get.map { topCities }
      _ <- storedRanking.set(newRanking)
    } yield ()).foreverM
  }

  def storeCheckIn(storedCheckIns: Ref[IO, Map[City, Int]])(city: City): IO[Unit] = {
    storedCheckIns.update {
      _.updatedWith(city) {
        case None        => Some(1)
        case Some(value) => Some(value + 1)
      }
    }
  }

  case class ProcessingCheckIns(currentRanking: IO[List[CityStats]], stop: IO[Unit])
}
