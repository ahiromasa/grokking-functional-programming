package chapter02

object TipCalculator {
  def getTipPercentage(names: List[String]): Int = {
    if names.size > 5 then { 20 }
    else if names.size > 0 then { 10 }
    else { 0 }
  }
}
