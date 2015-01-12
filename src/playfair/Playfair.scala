package playfair

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * Created by Yakov Boglev on 11/01/2015.
 */
object Playfair {
  /**
   * Reads in user's keyword from the console
   * @return String of user input
   */
  def obtainKeywordFromUser(): String = Console.in.readLine()

  /**
   * Reads in user's filename location from the console
   * @return the contents of a file that may or may not exist or be valid
   */
  def obtainFileFromUser(): Try[String] = {
    val name = Console.in.readLine()
    Try(Source.fromFile(name).mkString)
  }

  /**
   * Checks a strong to see whether it exists, and if so whether it contains valid text
   * that can be encoded
   * @param text the string to check
   * @return true if exists and valid for encoding; false otherwise
   */
  def checkFileValidForEncoding(text: Try[String]): Boolean = text match {
    case Success(s) => s.toList.count((_: Char).isLetter) > 1
    case Failure(s) => false
  }

  /**
   * Checks a strong to see whether it exists, and if so whether it contains valid text
   * that can be decoded
   * @param text the string to check
   * @return true if exists and valid for decoding; false otherwise
   */
  def checkFileValidForDecoding(text: Try[String]): Boolean = text match {
    case Success(s) =>
      val lst = s.toList.filter(_.isLetter)
      lst.nonEmpty && (lst.size % 2 ==0) // text is all letters and has an even number of letters
    case Failure(s) => false
  }

  /**
   * Checks user's keyword to ensure it has no non-letter characters, and is not empty
   * @param word user's keyword
   * @return Failure[IllegalArgumentException] if tests fail; Success[keyword as list of chars] otherwise
   */
  def checkKeyword(word: String): Try[String]  = {
    val keyword = word.toList.filter(c => !c.isSpaceChar)
    if (keyword.isEmpty) Failure(new IllegalArgumentException("The supplied keyword is empty."))
    else if (keyword.exists(c => !c.isLetter)) Failure(new IllegalArgumentException("The supplied keyword contains non-letter characters."))
    else Success(word.filter(c => !c.isSpaceChar))
  }


}
