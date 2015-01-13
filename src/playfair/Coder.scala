package playfair

/**
 * Created by Yakov Boglev on 12/01/2015.
 */
class Coder(keyword: String) {
  val alphabet = "abcdefghiklmnopqrstuvwxyz".toList
  val rowBounds = Array(0, 5, 10, 15, 20, 25)
  val codeBlock = createCodeBlock()

  def encode(plaintText: String): String = {
    def encodeHelper(lst: List[Char], acc: List[Char]): List[Char] = lst match {
      case Nil                            => acc.reverse
      case a::Nil                         => encodeHelper(Nil, processLetters(a, 'z') ::: acc)
      case a::b::rest if a==b && a!='x'   => encodeHelper(b::rest, processLetters(a, 'x') ::: acc)
      case a::b::rest if a==b && a=='x'   => encodeHelper(b::rest, processLetters(a, 'q') ::: acc)
      case a::b::rest if a!=b             => encodeHelper(rest, processLetters(a, b) ::: acc)
    }
    def processLetters(x: Char, y: Char): List[Char] = {
      def sameRow(i: Int, j: Int): Boolean = {
        val big = math.max(i, j)
        val small = math.min(i, j)
        rowBounds.indexWhere(p =>  p <= big && p + 1 > big) == rowBounds.indexWhere(p =>  p <= small && p + 1 > small)
      }
      def sameColumn(i: Int, j: Int): Boolean = {
        i%5 == j%5
      }

      val i = codeBlock indexOf x
      val j = codeBlock indexOf y

      // requires reverse
    }
  }

  def decode(secretText: String): String = ???

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
}
