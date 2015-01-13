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
  def obtainKeywordFromUser(): Try[String] = Try(Console.in.readLine())

  /**
   * Reads in user's filename location from the console
   * @return the contents of a file that may or may not exist or be valid
   */
  def obtainFileFromUser(): Try[String] = {
    val f = Source.fromFile( Console.in.readLine() )
    val result = Try(f.mkString)
    f.close()
    result
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
  def checkKeyword(word: Try[String]): Boolean = word match {
    case Success(w) =>
      val wNoSpaces = w.toList.filter(!_.isSpaceChar)
      wNoSpaces.nonEmpty && !wNoSpaces.exists(!_.isLetter)
    case Failure(w) => false
  }

  def main(args: Array[String]) {
    var finished = false
    while (!finished) {
      println("Choose your option from the list below:")
      println("\tTo encode a text press:\t\t'e'")
      println("\tTo decode a text press:\t\t'd'")
      println("\tTo quit press:\t\t\t'q'")
      processOption()
    }


    def processOption(): Any = {
      val option = Console.in.readLine()
      option match {
        case "e"  =>  doEncodeSteps(file, key.get)
        case "d"  =>  doDecodeSteps(file, key.get)
        case "q"  =>  finished = true
        case _    =>  println("\nBad input. Try again")
      }

      else {
        println()
        print("Enter the name of your target file, including extension (and path if in another directory):  ")
        val file = obtainFileFromUser()
      }
    }
    def doKeywordSteps() = {
      print("Enter keyword:\t\t")
      val key = obtainKeywordFromUser()
      if (!checkKeyword(key)) println("Bad keyword. Try again")


    }
    def doEncodeSteps(f: Try[String], k: String) = {
      if (!checkFileValidForEncoding(f)) println("Bad file or filename. Try again")
      else {
        val c = new Coder(k)
        c.display(c.encode(f.get))
      }
    }
    def doDecodeSteps(f: Try[String], k: String) = {
      if (!checkFileValidForDecoding(f)) println("Bad file or filename. Try again")
      else {
        val c = new Coder(k)
        c.display(c.decode(f.get))
      }
    }

  }

}
