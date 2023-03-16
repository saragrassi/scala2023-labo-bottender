package Chat

import Chat.Token.*
import Utils.SpellCheckerService

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  /** Separate the user's input into tokens
    * @param input
    *   The user's input
    * @return
    *   A Tokenizer which allows iteration over the tokens of the input
    */
  // TODO - Part 1 Step 3
  def tokenize(input: String): Tokenized =
    val tokens =
      input
        .replaceAll("[.,!?*'']", " ")
        .split("\\s+")
        // .toList
        .map(w => spellCheckerSvc.getClosestWordInDictionary(w))
        .map(w => toTuple(w))
        .toArray
    TokenizedImpl(tokens)
  end tokenize

  def toTuple(word: String): (String, Token) =
    word match {
      case "bonjour"                => ("bonjour", Token.BONJOUR)
      case "je"                     => ("je", Token.JE)
      case "etre"                   => ("etre", Token.ETRE)
      case "vouloir"                => ("vouloir", Token.VOULOIR)
      case "assoiffe"               => ("assoiffe", Token.ASSOIFFE)
      case "affame"                 => ("affame", Token.AFFAME)
      case "biere"                  => ("biere", Token.PRODUIT)
      case "croissant"              => ("croissant", Token.PRODUIT)
      case "et"                     => ("et", Token.ET)
      case "ou"                     => ("ou", Token.OU)
      case "svp"                    => ("svp", Token.SVP)
      case w if w.startsWith("_")   => (w, Token.PSEUDO)
      case w if w.forall(_.isDigit) => (w, Token.NUM)
      case _                        => (word, Token.UNKNOWN)
    }

end TokenizerService
