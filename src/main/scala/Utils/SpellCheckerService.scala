package Utils

trait SpellCheckerService:
  /** This dictionary is a Map object that contains valid words as keys and
    * their normalized equivalents as values (e.g. we want to normalize the
    * words "veux" and "aimerais" in one unique term: "vouloir").
    */
  val dictionary: Map[String, String]

  /** Calculate the Levenstein distance between two words.
    * @param s1
    *   the first word
    * @param s2
    *   the second word
    * @return
    *   an integer value, which indicates the Levenstein distance between "s1"
    *   and "s2"
    */
  def stringDistance(s1: String, s2: String): Int

  /** Get the syntactically closest word in the dictionary from the given
    * misspelled word, using the "stringDistance" function. If the word is a
    * number or a pseudonym, this function just returns it.
    * @param misspelledWord
    *   the mispelled word to correct
    * @return
    *   the closest normalized word from "mispelledWord"
    */
  def getClosestWordInDictionary(misspelledWord: String): String
end SpellCheckerService

class SpellCheckerImpl(val dictionary: Map[String, String])
    extends SpellCheckerService:

  // TODO - Part 1 Step 2
  def stringDistance(str1: String, str2: String): Int =
    def lev(a: String, b: String): Int = {
      if (a.length.min(b.length) == 0) a.length.max(b.length)
      else if (a.head == b.head) lev(a.tail, b.tail)
      else (1 + lev(a.tail, b).min(lev(a, b.tail)).min(lev(a.tail, b.tail)))
    }
    lev(str1, str2)
  end stringDistance

  // TODO - Part 1 Step 2
  def getClosestWordInDictionary(misspelledWord: String): String =

    // deal with number, pseudo, and word in dictionnary
    if (misspelledWord.forall(_.isDigit)) return misspelledWord
    if (misspelledWord.startsWith("_")) return misspelledWord
    if (dictionary.contains(misspelledWord)) return dictionary(misspelledWord)

    // find the (alphabetically lowest) key with the lowest score
    val lst = dictionary.keys
      .map(w => (stringDistance(misspelledWord, w), w))
      .toList
    val (minKey, minValue) = lst.minBy(_._1)
    val key = lst.filter(_._1 == minKey).sortBy(_._2).head._2
    return dictionary(key) // return normalized version

  end getClosestWordInDictionary

end SpellCheckerImpl
