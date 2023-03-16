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
    val len1 = str1.length
    val len2 = str2.length
    val matrix = Array.ofDim[Int](len1 + 1, len2 + 1)
    for i <- 0 to len1 do matrix(i)(0) = i
    for j <- 0 to len2 do matrix(0)(j) = j
    for i <- 1 to len1; j <- 1 to len2 do
      val cost = if (str1(i - 1) == str2(j - 1)) 0 else 1
      matrix(i)(j) = math.min(
        math.min(matrix(i - 1)(j) + 1, matrix(i)(j - 1) + 1),
        matrix(i - 1)(j - 1) + cost
      )
    matrix(len1)(len2)
  end stringDistance

  // TODO - Part 1 Step 2
  def getClosestWordInDictionary(misspelledWord: String): String =
    // Check if the word is a number
    if (misspelledWord.forall(_.isDigit)) return misspelledWord
    // Check if the word is a pseudonym
    if (misspelledWord.startsWith("_")) return misspelledWord
    // return the normalized word if the word is in the dictionary
    if (dictionary.contains(misspelledWord)) return dictionary(misspelledWord)

    // find the lowest key with the lowest score
    val lst = dictionary.keys
      .map(w => (stringDistance(misspelledWord, w), w))
      .toList
    val (minKey, minValue) = lst.minBy(_._1)
    val key = lst.filter(_._1 == minKey).sortBy(_._2).head._2
    return dictionary(key)

  end getClosestWordInDictionary

end SpellCheckerImpl
