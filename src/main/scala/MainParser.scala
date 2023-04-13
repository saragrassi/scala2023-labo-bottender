import scala.io.StdIn
import Utils._
import Chat.{
  TokenizerService,
  Token,
  UnexpectedTokenException,
  Parser,
  AnalyzerService
}
import Data._

import scala.io.StdIn

object MainParser:
  /** Convert the user input to lower case, then take an action depending on the
    * value.
    *
    * @param tokenizerSvc
    *   The service used to tokenize the input
    * @param analyzerSvc
    *   The service used to analyze the expression tree
    * @param session
    *   The session instance that stores
    * @param input
    *   The user input
    * @return
    *   whether the input loop should continue or not
    */
  def evaluateInput(
      tokenizerSvc: TokenizerService,
      analyzerSvc: AnalyzerService,
      session: Session
  )(input: String): Boolean =
    input.toLowerCase match
      case "quitter" =>
        println("Adieu.")
        false // close loop
      case "santé !" =>
        for i <- 2 to 6 do
          println(
            s"Nombre de *clinks* pour un santé de $i personnes : ${ClinksCalculator
                .calculateCombination(i, 2)}."
          )
        true // continue loop
      case s =>
        try
          val tokenized = tokenizerSvc.tokenize(s)

          val parser = new Parser(tokenized)
          val expr = parser.parsePhrases()

          val printResult = analyzerSvc.reply(session)(expr)

          println(printResult)
        catch
          case e: UnexpectedTokenException =>
            println(s"Invalid input. ${e.getMessage}")
        true // continue loop
  end evaluateInput

  def main(args: Array[String]): Unit =
    val spellCheckerSvc: SpellCheckerService = new SpellCheckerImpl(
      Dictionary.dictionary
    )
    val tokenizerSvc: TokenizerService = new TokenizerService(spellCheckerSvc)
    val productSvc: ProductService = new ProductImpl()
    val accountSvc: AccountService = new AccountImpl()
    val sessionSvc: SessionService = new SessionImpl()
    val analyzerSvc: AnalyzerService =
      new AnalyzerService(productSvc, accountSvc)

    println("Bienvenue au Chill-Out !")

    val session = sessionSvc.create()

    var i = 0
    while
      print("> ")
      evaluateInput(tokenizerSvc, analyzerSvc, session)(StdIn.readLine)
      // var line = testPhrases(i % testPhrases.length)
      // i += 1
      // println(line)
      // evaluateInput(tokenizerSvc, analyzerSvc, session)(line)
    do ()
  end main

  private val testPhrases = List(
    "Je suis assoiffé !",
    "Je suis affamé !",
    "Bonjour, je suis assoiffé !",
    "J'aimerais commander 3 bières PunkIPAs !",
    "Je suis _Michel.",
    "Combien coûte 1 bière PunkIPA ?",
    "J'aimerais connaitre mon solde.",
    "Je veux commander 2 bières PunkIPAs et 1 bière Ténébreuse.",
    "Je voudrais commander 1 croissant.",
    "Bonjour ! Je suis _Bobby.",
    "Je suis affamé !",
    "Je veux commander 2 croissants cailler.",
    "Je veux connaître mon solde.",
    "Je suis _Michel.",
    "Je veux connaïtre mon solde.",
    "J'aimerais commander 18 bières Farmer.",
    "J'aimerais commander 1 bière.",
    "Santé !",
    "Combien coute 1 bière PunkIPA et 1 bière boxer ou 1 biere farmer ou 1 bière tenebreuse et 1 bière boxer?",
    "Quitter"
  )

end MainParser
