package Chat

class UnexpectedTokenException(msg: String) extends Exception(msg) {}

class Parser(tokenized: Tokenized):
  import ExprTree._
  import Chat.Token._

  // Start the process by reading the first token.
  var curTuple: (String, Token) = tokenized.nextToken()

  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = tokenized.nextToken()

  /** "Eats" the expected token and returns it value, or terminates with an
    * error.
    */
  private def eat(token: Token): String =
    if token == curToken then
      val tmp = curValue
      readToken()
      tmp
    else expected(token)

  /** Complains that what was found was not expected. The method accepts
    * arbitrarily many arguments of type Token
    */
  private def expected(token: Token, more: Token*): Nothing =
    expected(more.prepended(token))
  private def expected(tokens: Seq[Token]): Nothing =
    val expectedTokens = tokens.mkString(" or ")
    throw new UnexpectedTokenException(
      s"Expected: $expectedTokens, found: $curToken"
    )

  /** the root method of the parser: parses an entry phrase */
  // TODO - Part 2 Step 4

  // parse: ["bonjour"] ("quel" | "combien" | "je")
  def parsePhrases(): ExprTree =
    if curToken == BONJOUR then readToken()
    curToken match
      case QUEL    => readToken(); parseQuel()
      case COMBIEN => readToken(); parseCombien()
      case JE      => readToken(); parseJe()
      case EOL     => readToken(); Greeting;
      case _       => expected(JE, QUEL, COMBIEN)

  // parse: "quel" "etre" "le" "prix" "de" "produits"
  def parseQuel(): ExprTree =
    eat(ETRE)
    eat(LE)
    eat(PRIX)
    eat(DE)
    val products = parseProducts()
    Price(products)

  // parse: "combien" "couter" "produits"
  def parseCombien(): ExprTree =
    eat(COUTER)
    val products = parseProducts()
    Price(products)

  // parse: "num" "produit" ["marque"]
  def parseProduct(): ExprTree =
    val num = eat(NUM).toInt
    val product = eat(PRODUIT)
    if curToken == MARQUE then Product(num, product, Some(eat(MARQUE)))
    else Product(num, product, None)

  // parse: "produits" ("et" "produits" | "ou" "produits" | EOL)
  def parseProducts(): ExprTree =
    val firstProduct = parseProduct()
    curToken match
      case ET =>
        eat(ET)
        And(firstProduct, parseProducts())
      case OU =>
        eat(OU)
        Or(firstProduct, parseProducts())
      case EOL =>
        firstProduct
      case _ => expected(ET, OU, EOL)

  // parse the "je" ("vouloir" | "etre" | "me") phrases
  def parseJe(): ExprTree =
    curToken match
      case VOULOIR => readToken(); parseVouloir()
      case ETRE    => readToken(); parseEtre()
      case ME      => readToken(); parseMe()
      case _       => expected(ETRE, VOULOIR, ME)

  // parse: "je vouloir" ("commander" | "connaître")
  def parseVouloir(): ExprTree =
    curToken match
      case COMMANDER => readToken(); parseCommander()
      case CONNAITRE => readToken(); parseConnaitre()
      case _         => expected(COMMANDER, CONNAITRE)

  // parse: "je vouloir commander" "produits"
  def parseCommander(): ExprTree =
    val products = parseProducts()
    Order(products)

  // parse: "je vouloir connaître" "mon" "solde"
  def parseConnaitre(): ExprTree =
    eat(MON)
    eat(SOLDE)
    GetBalance

  // parse: "je etre" ("assoiffé" | "affamé" | "pseudo")
  def parseEtre(): ExprTree =
    curToken match
      case ASSOIFFE => readToken(); Thirsty
      case AFFAME   => readToken(); Hungry
      case PSEUDO   => Identification(eat(PSEUDO).stripPrefix("_"))
      case _        => expected(ASSOIFFE, AFFAME, PSEUDO)

  // parse: "je me" "appeler" "pseudo"
  def parseMe(): ExprTree =
    eat(APPELLER)
    Identification(eat(PSEUDO).stripPrefix("_"))
