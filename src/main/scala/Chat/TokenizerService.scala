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
  // TODO - Part 2 Step 1
  def tokenize(input: String): Tokenized =
    val tokens =
      input
        .replaceAll("[.,!?*']", " ")
        .split("\\s+")
        .map(w => spellCheckerSvc.getClosestWordInDictionary(w))
        .map(w => toTuple(w))
    TokenizedImpl(tokens)
  end tokenize

  def toTuple(word: String): (String, Token) =
    val marques = List("maison", "cailler");
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
      case "maison"                 => ("maison", Token.MARQUE)
      case "cailler"                => ("cailler", Token.MARQUE)
      case "farmer"                 => ("farmer", Token.MARQUE)
      case "boxer"                  => ("boxer", Token.MARQUE)
      case "wittekop"               => ("wittekop", Token.MARQUE)
      case "punkipa"                => ("punkipa", Token.MARQUE)
      case "jackhammer"             => ("jackhammer", Token.MARQUE)
      case "tenebreuse"             => ("tenebreuse", Token.MARQUE)
      case "commander"              => ("commander", Token.COMMANDER)
      case "combien"                => ("combien", Token.COMBIEN)
      case "coute"                  => ("coute", Token.COUTER)
      case "connaitre"              => ("connaitre", Token.CONNAITRE)
      case "mon"                    => ("mon", Token.MON)
      case "solde"                  => ("solde", Token.SOLDE)
      case "quel"                   => ("quel", Token.QUEL)
      case "le"                     => ("le", Token.LE)
      case "prix"                   => ("prix", Token.PRIX)
      case "de"                     => ("de", Token.DE)
      case "me"                     => ("de", Token.ME)
      case "appeler"                => ("appeler", Token.APPELLER)
      case w if w.startsWith("_")   => (w, Token.PSEUDO)
      case w if w.forall(_.isDigit) => (w, Token.NUM)
      case _                        => (word, Token.UNKNOWN)
    }

end TokenizerService
