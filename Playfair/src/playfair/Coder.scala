package playfair

/**
 * Created by Yakov Boglev on 12/01/2015.
 */
class Coder(keyword: String) {
  val alphabet = "abcdefghiklmnopqrstuvwxyz".toList
  val rowBounds = Array(0, 5, 10, 15, 20, 25)
  val codeBlock = createCodeBlock()
  val BLOCKS_PER_LINE = 10
  val LETTERS_PER_BLOCK = 5
  def messagePrep(text: String): List[Char] = {
    val textWithoutJ = text.map((c: Char) => if (c == 'j') 'i' else c)
    (for (c <- textWithoutJ if c isLetter) yield c.toLower).toList
  }

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

  def encode(plaintText: String): String = {
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

  def decode(secretText: String): String = {
    def decodeHelper(lst: List[Char], acc: List[Char]): List[Char] = {
      val decode = "decode"
      lst match {
        case Nil            => acc.reverse
        case a :: b :: rest => decodeHelper(rest, processLetters(a, b, decode) ::: acc)
      }
    }
    decodeHelper(messagePrep(secretText), List[Char]()).mkString
  }

  def display(output: String): String = ???

  def processLetters(x: Char, y: Char, direction: String): List[Char] = {
    def sameRow(i: Int, j: Int): Boolean = {
      val big = math.max(i, j)
      val small = math.min(i, j)
      if (rowBounds.indexWhere(indx => indx > small && indx <= big) == -1) true else false
    }
    def sameColumn(i: Int, j: Int): Boolean = {
      i%5 == j%5
    }
    def rowOperation(k: Int): Int = direction match {
      case "encode" => if ((k+1)%5 == 0) k - 4
      else k + 1
      case "decode" =>
        val indexMod = k % 5
        if (indexMod - 1 < 0) k + 4 else k - 1
    }
    def columnOperation(k: Int): Int = direction match {
      case "encode" => if (k >= 20) k - 20 else k + 5
      case "decode" => if (k <= 4) k + 20 else k - 5
    }
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
