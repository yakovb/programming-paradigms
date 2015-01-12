package playfair

/**
 * Created by Yakov Boglev on 12/01/2015.
 */
class Coder(keyword: String) {
  val alphabet = "abcdefghiklmnopqrstuvwxyz".toList
  val rowBounds = Array(0, 5, 10, 15, 20)

  def encode(plaintText: String): String = ???

  def decode(secretText: String): String = ???

  def createCodeBlock(): Array[Char] = ???
}
