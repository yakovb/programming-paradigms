/**
 * Created by Yakov Boglev on 11/01/2015.
 */

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

  it must "return nothing if not properly requested" in {}
  it must "return nothing if properly requested but doesn't contain any valid text" in {}
  it must "return a string if properly requested and contains valid text" in {}

}
