package chapter08

import cats.effect.IO
import cats.effect.unsafe.implicits.global

object CastingDie {
  def castTheDieImpure(): Int = ???

  def castTheDie(): IO[Int] = {
    IO.delay { castTheDieImpure() }
  }

  @main def runCastTheDie = {
    val dieCast: IO[Int] = castTheDie()
    dieCast.unsafeRunSync()
  }

  def castTheDieTwice(): IO[Int] = {
    for {
      firstCast <- castTheDie()
      secondCast <- castTheDie()
    } yield firstCast + secondCast
  }
}
