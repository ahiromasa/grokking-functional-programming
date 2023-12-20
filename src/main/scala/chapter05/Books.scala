package chapter05

object Books {
  case class Book(title: String, authors: List[String])

  val books = List(
    Book("FP in Scala", List("Chiusano", "Bjarnason")),
    Book("The Hobbit", List("Tolkien")),
    Book("Modern Java in Action", List("Urma", "Fusco", "Mycroft"))
  )

  case class Movie(title: String)

  def bookAdaptations(author: String): List[Movie] = {
    if author == "Tolkien" then { List(Movie("An Unexpected Journey"), Movie("The Desolation of Smaug")) }
    else { List.empty }
  }

  def recommendationFeed(books: List[Book]): List[String] = {
    for {
      book <- books
      author <- book.authors
      movie <- bookAdaptations(author)
    } yield s"You may like ${movie.title}, because you liked ${author}'s ${book.title}"
  }
}
