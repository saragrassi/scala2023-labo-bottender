package Utils

/** Contains the dictionary of the application, which is used to validate,
  * correct and normalize words entered by the user.
  */
object Dictionary:
  // This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
  // we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
  val dictionary: Map[String, String] = Map(
    "bonjour" -> "bonjour",
    "hello" -> "bonjour",
    "yo" -> "bonjour",
    "je" -> "je",
    "j" -> "je",
    "suis" -> "etre",
    "veux" -> "vouloir",
    "aimerais" -> "vouloir",
    "assoiffé" -> "assoiffe",
    "assoiffée" -> "assoiffe",
    "affamé" -> "affame",
    "affamée" -> "affame",
    "bière" -> "biere",
    "bières" -> "biere",
    "croissant" -> "croissant",
    "croissants" -> "croissant",
    "et" -> "et",
    "ou" -> "ou",
    "svp" -> "svp",
    "stp" -> "svp",
    // TODO - Part 2 Step 1
    "maison" -> "maison",
    "cailler" -> "cailler",
    "farmer" -> "farmer",
    "boxer" -> "boxer",
    "wittekop" -> "wittekop",
    "punkipa" -> "punkipa",
    "jackhammer" -> "jackhammer",
    "ténébreuse" -> "tenebreuse",
    "ténebreuse" -> "tenebreuse",
    "tenébreuse" -> "tenebreuse",
    "tenebreuse" -> "tenebreuse",
    "commander" -> "commander",
    "combien" -> "combien",
    "coûte" -> "coute",
    "coute" -> "coute",
    "connaître" -> "connaitre",
    "connaitre" -> "connaitre",
    "mon" -> "mon",
    "solde" -> "solde",
    "m" -> "me"
  )

end Dictionary
