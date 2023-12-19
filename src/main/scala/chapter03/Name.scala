package chapter03

object Name {
  def abbreviate(name: String): String = {
    val initial = name.substring(0, 1)
    val separator = name.indexOf(' ')
    val lastName = name.substring(separator + 1)
    initial + ". " + lastName
  }
}
