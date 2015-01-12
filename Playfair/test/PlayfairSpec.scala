/**
 * Created by Yakov Boglev on 11/01/2015.
 */

import java.io.FileNotFoundException

import org.scalatest.FlatSpec
import playfair.Playfair._

class PlayfairSpec extends FlatSpec {

  /**
   * Test the behaviour of checkKeyword. This should accept words or phrases with letters only
   * and can include spaces (in addition to letters). In this case it should return a Success[String];
   * in all other cases it should return a Failure[String],
   */
  behavior of "a keyword"

  it must "not be an empty string" in {
    assert(checkKeyword("").isFailure)
  }
  it must "not consist solely of spaces" in {
    assert(checkKeyword("   ").isFailure)
  }
  it must "not contain any non-letter characters (except spaces)" in {
    assert(checkKeyword("my-keyword").isFailure)
  }
  it can "be a long string that includes spaces" in {
    val longKey = "this is a very long keyword that I would like to use"
    assert(checkKeyword(longKey).get === longKey.filter(c => !c.isSpaceChar))
  }
  it can "be a single word" in {
    val k = "keyword"
    assert(checkKeyword(k).get === k)
  }
  it can "be the word Pennsylvania" in {
    val k = "Pennsylvania"
    assert(checkKeyword(k).get === k)
  }

  /**
   * Test the behaviour of the file getting methods. A correctly specified file should return a String
   * while an incorrectly specified filename should return a nothing - this is implemented via Option.
   */

  behavior of "a source file"

  it must "return an Option enclosing a FileNotFoundException if not properly requested" in {
    intercept[FileNotFoundException] {
      obtainFileFromUser("badFileName").get
    }
  }
  it must "return an Option enclosing an IllegalArgumentException if properly requested but file is empty" in {
    intercept[IllegalArgumentException] {
      obtainFileFromUser("invalid-empty.txt").get
    }
  }
  it must "return an Option enclosing an IllegalArgumentException if properly requested but contains no valid text" in {
    intercept[IllegalArgumentException] {
      obtainFileFromUser("invalid-noletters.txt").get
    }
  }
  it must "return an Option enclosing an IllegalArgumentException if properly requested but contains fewer than two letters" in {
    intercept[IllegalArgumentException] {
      obtainFileFromUser("invalid-oneletter.txt").get
    }
  }
  it must "return a string if properly requested and contains valid but meaningless text" in {
    assert(obtainFileFromUser("valid-twoletters.txt").get.matches(".*d.*R.*"))
  }
  it must "return a string if properly requested and contains valid text" in {
    assert(obtainFileFromUser("wiki-plaintext.txt").get === "Hide the gold in the tree stump")
  }

}
