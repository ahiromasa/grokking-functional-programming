package chapter09

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream

object CastingDieStream {
  def castTheDieImpure(): Int = ???

  def castTheDie(): IO[Int] = {
    IO.delay { castTheDieImpure() }
  }

  val dieCast: Stream[IO, Int] = Stream.eval(castTheDie())
  val oneDieCastProgram: IO[List[Int]] = dieCast.compile.toList

  @main def runOneDieCastProgram = {
    oneDieCastProgram.unsafeRunSync()
  }

  val infiniteDieCasts: Stream[IO, Int] = Stream.eval(castTheDie()).repeat
  val infiniteDieCastsProgram: IO[Unit] = infiniteDieCasts.compile.drain

  val firstThreeCast: IO[List[Int]] = infiniteDieCasts.take(3).compile.toList

  val six: IO[List[Int]] = infiniteDieCasts.filter { _ == 6 }.take(1).compile.toList
}
