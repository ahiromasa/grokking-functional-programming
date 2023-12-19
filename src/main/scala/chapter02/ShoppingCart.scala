package chapter02

object ShoppingCart {
  def getDiscountPercentage(items: List[String]): Int = {
    if items.contains("Book") then { 5 }
    else { 0 }
  }
}
