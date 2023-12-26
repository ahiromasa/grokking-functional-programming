package chapter10

import cats.effect.IO
import cats.effect.Ref
import cats.implicits.*

import concurrent.duration.*

object CastingDieConcurrently {
  def castTheDie(): IO[Int] = ???

  for {
    _ <- IO.sleep(1.second)
    result <- List(castTheDie(), castTheDie()).parSequence
  } yield result.sum

  for {
    storedCasts <- Ref.of[IO, List[Int]](List.empty)
    singleCast <- castTheDie().flatMap { result => storedCasts.update { _.appended(result) } }
    _ <- List(castTheDie(), castTheDie()).parSequence
    casts <- storedCasts.get
  } yield casts

  for {
    storedCasts <- Ref.of[IO, List[Int]](List.empty)
    singleCast = castTheDie().flatMap { result => storedCasts.update { _.appended(result) } }
    _ <- List.fill(3)(singleCast).parSequence
    casts <- storedCasts.get
  } yield casts

  for {
    storedCasts <- Ref.of[IO, Int](0)
    singleCast = castTheDie().flatMap { result =>
      if result == 6 then storedCasts.update { _ + 1 }
      else IO.unit
    }
    _ <- List.fill(100)(singleCast).parSequence
    casts <- storedCasts.get
  } yield casts

  List
    .fill(100) { IO.sleep(1.second).flatMap { _ => castTheDie() } }
    .parSequence
    .map { _.sum }
}
