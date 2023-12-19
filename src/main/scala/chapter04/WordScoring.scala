package chapter04

object WordScoring {
  def rankedWords(wordScore: String => Int)(words: List[String]): List[String] = {
    words.sortBy { wordScore }.reverse
  }

  def wordScores(wordScore: String => Int)(words: List[String]): List[Int] = {
    words.map { wordScore }
  }

  def highScoringWords(wordScore: String => Int)(higherThan: Int)(words: List[String]): List[String] = {
    words.filter { wordScore(_) > higherThan }
  }

  def cumulativeScore(wordScore: String => Int)(words: List[String]): Int = {
    words.foldLeft(0) { _ + wordScore(_) }
  }
}
