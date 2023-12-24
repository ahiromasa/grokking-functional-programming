package chapter09

import cats.effect.IO
import cats.implicits.*
import fs2.Stream

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object CurrencyExchange {
  def exchangeRatesTableApiCall(currency: String): Map[String, BigDecimal] = ???

  object model {
    opaque type Currency = String

    object Currency {
      def apply(name: String): Currency = name

      extension (self: Currency) {
        def name: String = self
      }
    }
  }

  import model.*

  def exchangeTable(from: Currency): IO[Map[Currency, BigDecimal]] = {
    IO
      .delay { exchangeRatesTableApiCall(from.name) }
      .map { _.map { case (currencyName, rate) => (Currency(currencyName), rate) } }
  }

  def trending(rates: List[BigDecimal]): Boolean = {
    rates.size > 1 && rates.zip(rates.drop(1)).forall { case (previousRate, rate) => rate > previousRate }
  }

  def extractSingleCurrencyRate(currencyToExtract: Currency)(table: Map[Currency, BigDecimal]): Option[BigDecimal] = {
    // table.filter { case (currency, rate) => currency == currencyToExtract }.values.headOption
    table.get(currencyToExtract)
  }

  def retry[A](action: IO[A], maxRetries: Int): IO[A] = {
    List
      .range(0, maxRetries)
      .map { _ => action }
      .foldLeft(action) { (program, retryAction) => program.orElse { retryAction } }
  }

  def lastRates(from: Currency, to: Currency, n: Int): IO[List[BigDecimal]] = {
    List.range(0, n).map { _ => currencyRate(from, to) }.sequence
  }

  def exchangeIfTrending(amount: BigDecimal, from: Currency, to: Currency): IO[BigDecimal] = {
    rates(from, to)
      .zipLeft(ticks)
      .sliding(3)
      .map { _.toList }
      .filter { trending }
      .map { _.last }
      .compile
      .lastOrError
      .map { _ * amount }
  }

  def currencyRate(from: Currency, to: Currency): IO[BigDecimal] = {
    for {
      table <- retry(exchangeTable(from), 10)
      rate <- extractSingleCurrencyRate(to)(table) match {
        case Some(value) => IO.pure(value)
        case None        => currencyRate(from, to)
      }
    } yield rate
  }

  def rates(from: Currency, to: Currency): Stream[IO, BigDecimal] = {
    Stream
      .eval(exchangeTable(from))
      .repeat
      .map { extractSingleCurrencyRate(to) }
      .unNone
      .orElse { rates(from, to) }
  }

  val delay: FiniteDuration = FiniteDuration(1, TimeUnit.SECONDS)
  val ticks: Stream[IO, Unit] = Stream.fixedRate(delay)

  val firstThreeRates: IO[List[BigDecimal]] =
    rates(Currency("USD"), Currency("EUR"))
      .zipLeft(ticks)
      .take(3)
      .compile
      .toList
}
