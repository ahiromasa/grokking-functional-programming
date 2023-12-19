package chapter01

object Intro {
  def increment(x: Int): Int = {
    x + 1
  }

  def firstChar(x: String): Char = {
    x.charAt(0)
  }

  def wordScore(x: String): Int = {
    x.length()
  }
}

@main def run = {
  import chapter01.Intro.*

  assert { increment(6) == 7 }
  assert { firstChar("Ola") == 'O' }
  assert { wordScore("Scala") == 5 }
}
