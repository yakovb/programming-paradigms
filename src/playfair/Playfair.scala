package playfair

import scala.util.{Failure, Success}

/**
 * Created by Yakov Boglev on 11/01/2015.
 */
object Playfair {

  def obtainKeywordFromUser(): String = {
    Console.in.readLine()
  }

  def checkKeyword(word: String) = {
    val keyword = word.toList.filter(c => !c.isSpaceChar)
    if (keyword.isEmpty) Failure(new IllegalArgumentException("The supplied keyword is empty."))
    else if (keyword.exists(c => !c.isLetter)) Failure(new IllegalArgumentException("The supplied keyword contains non-letter characters."))
    else Success(keyword)
  }
}
