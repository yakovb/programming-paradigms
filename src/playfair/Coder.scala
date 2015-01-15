package playfair

/**
 * Created by Yakov Boglev on 12/01/2015.
 */
class Coder(keyword: String) {
  val alphabet = "abcdefghiklmnopqrstuvwxyz".toList
  val rowBounds = Array(0, 5, 10, 15, 20, 25)
  val codeBlock = createCodeBlock()
  val blocksPerLine = 10
  val lettersPerBlock = 5

  /**
   * Prepares a message for encoding/decoding by changing j's to i's and removing all non-letter characters (including spaces)
   * @param text the text to be prepared for encoding/decoding
   * @return character list of the text, ready to be encoded/decoded
   */
  def messagePrep(text: String): List[Char] = {
    val textWithoutJ = text.map((c: Char) => if (c == 'j') 'i' else c)
    (for (c <- textWithoutJ if c isLetter) yield c.toLower).toList
  }

  /**
   * Creates the 5z5 code block according to the rules of the Playfair cipher
   * @return an array of characters representing the code block going left to right, row by row downwards
   */
  def createCodeBlock(): Array[Char] = {
    def blockHelper(lst: List[Char]): List[Char] = lst match {
      case Nil                  => lst
      case ('i' | 'j') :: rest  => 'i' :: blockHelper(lst.tail.filter(c => c != 'i' && c != 'j'))
      case h :: rest            => h :: blockHelper(lst.tail.filter(_ != h))
    }
    val keyPart = blockHelper(keyword.toLowerCase.toList).toArray
    val alphabetPart = (for (c <- alphabet if !keyPart.contains(c)) yield c).toArray
    keyPart ++ alphabetPart
  }

  /**
   * Encodes a given plaintext and returns display-ready output
   * @param plainText the text to be encoded
   * @return formatted string of the encoded text
   */
  def encode(plainText: String): String = display(encodeWoutFormatting(plainText))

  /**
   * Decodes a given secret text and returns display-ready output
   * @param secretText the text to be decoded
   * @return formatted string of the decoded text
   */
  def decode(secretText: String): String = display(decodeWoutFormatting(secretText))

  /**
   * Encodes a given plaintext without applying any formatting rules
   * @param plaintText the text to be encoded
   * @return the encoded text as a continuous string without spaces or newlines
   */
  def encodeWoutFormatting(plaintText: String): String = {
    def encodeHelper(lst: List[Char], acc: List[Char]): List[Char] = {
      val encode = "encode"
      lst match {
        case Nil => acc.reverse
        case a :: Nil => encodeHelper(Nil, processLetters(a, 'z', encode) ::: acc)
        case a :: b :: rest if a == b && a != 'x' => encodeHelper(b :: rest, processLetters(a, 'x', encode) ::: acc)
        case a :: b :: rest if a == b && a == 'x' => encodeHelper(b :: rest, processLetters(a, 'q', encode) ::: acc)
        case a :: b :: rest if a != b => encodeHelper(rest, processLetters(a, b, encode) ::: acc)
      }
    }
    encodeHelper(messagePrep(plaintText), List[Char]()).mkString
  }

  /**
   * Decodes a given secret text without applying any formatting rules
   * @param secretText the text to be decoded
   * @return the decoded text as a continuous string without spaces or newlines
   */
  def decodeWoutFormatting(secretText: String): String = {
    def decodeHelper(lst: List[Char], acc: List[Char]): List[Char] = {
      val decode = "decode"
      lst match {
        case Nil            => acc.reverse
        case a :: b :: rest => decodeHelper(rest, processLetters(a, b, decode) ::: acc)
        case _              => throw new Exception("badly formed secret text")
      }
    }
    decodeHelper(messagePrep(secretText), List[Char]()).mkString
  }

  /**
   * Displays encoded/decoded text according to set formatting rules,
   * i.e. letters per block and blocks per line
   * @param text the text to be formatted
   * @return formatted string of output text
   */
  def display(text: String): String = {
    val strBld1 = new StringBuilder
    val strBld2 = new StringBuilder
    val textBlocks = for (blockLngth <- Range(0, text.size, lettersPerBlock)) yield text.drop(blockLngth).take(lettersPerBlock)
    val textWithSpaces: String = textBlocks.toList.addString(strBld1, " ").mkString
    if (textWithSpaces.size < 60) textWithSpaces
    else {
      val lineLength = blocksPerLine * (lettersPerBlock + 1) // +1 for the space at the end of the line
      val textLines = for (lineLngth <- Range(0, textWithSpaces.size, lineLength)) yield textWithSpaces.drop(lineLngth).take(lineLength)
      val textLinesTrimmed = textLines.map(ln => if (ln.size % lineLength == 0) ln.init else ln)
      textLinesTrimmed.toList.addString(strBld2, "\n").mkString
    }
  }

  /**
   * Checks where letters belong in the code block and how they should be en/decoded
   * @param x first letter of digraph
   * @param y second letter of digraph
   * @param direction whether the letters should be encoded or decoded
   * @return List of characters representing the en/decoded letters. Returned in reverse order because calling method
   *         builds the output string like a stack and then reverses it.
   */
  private def processLetters(x: Char, y: Char, direction: String): List[Char] = {
    /**
     * Checks whether 2 letters are in the same row of the code block
     * @param i index of the first letter
     * @param j index of the second letter
     * @return true if same row, false otherwise
     */
    def sameRow(i: Int, j: Int): Boolean = {
      val big = math.max(i, j)
      val small = math.min(i, j)
      if (rowBounds.indexWhere(indx => indx > small && indx <= big) == -1) true else false
    }
    /**
     * Checks whether 2 letters are in the same column of the code block
     * @param i index of first letter
     * @param j index of second letter
     * @return true if same column, false otherwise
     */
    def sameColumn(i: Int, j: Int): Boolean = {
      i%5 == j%5
    }
    /**
     * Encodes/decodes a letter using the 'row rule' of the Playfair cipher
     * @param k the letter to be encoded/decoded
     * @return the encoded/decoded letter
     */
    def rowOperation(k: Int): Int = direction match {
      case "encode" => if ((k+1)%5 == 0) k - 4
      else k + 1
      case "decode" =>
        val indexMod = k % 5
        if (indexMod - 1 < 0) k + 4 else k - 1
    }
    /**
     * Encodes/decodes a letter using the 'column rule' of the Playfair cipher
     * @param k the letter to be encoded/decoded
     * @return the encoded/decoded letter
     */
    def columnOperation(k: Int): Int = direction match {
      case "encode" => if (k >= 20) k - 20 else k + 5
      case "decode" => if (k <= 4) k + 20 else k - 5
    }
    /**
     * Encodes/decodes two letters using the 'rectangle rule' of the Playfair cipher
     * @param i index of the first letter
     * @param j index of the second letter
     * @return the encoded/decoded letters as a list
     */
    def rectangleOperation(i: Int, j: Int): List[Char] = {
      val iDiff = 4 - (i%5)
      val jDiff = 4 - (j%5)
      val absDiff = math.abs(iDiff - jDiff)
      if (iDiff > jDiff) List(codeBlock(i + absDiff), codeBlock(j - absDiff))
      else List(codeBlock(i - absDiff), codeBlock(j + absDiff))
    }
    val i = codeBlock indexOf x
    val j = codeBlock indexOf y

    // return new letters in reverse order as the output string is built like a stack
    if (sameRow(i, j)) List(codeBlock(rowOperation(j)), codeBlock(rowOperation(i)))
    else if (sameColumn(i, j)) List(codeBlock(columnOperation(j)), codeBlock(columnOperation(i)))
    else rectangleOperation(i, j).reverse
  }

}
