import org.scalatest.*
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpec
import prop.*

import java.io.ByteArrayOutputStream
import Utils.{
  Dictionary,
  SpellCheckerService,
  SpellCheckerImpl,
  ClinksCalculator
}
import Chat.TokenizerService

class BotTenderTokenizerInputSuite
    extends AnyPropSpec
    with TableDrivenPropertyChecks
    with should.Matchers {
  val spellCheckerSvc: SpellCheckerService = new SpellCheckerImpl(
    Dictionary.dictionary
  )
  val tokenizerSvc: TokenizerService = new TokenizerService(spellCheckerSvc)

  val evaluateInput = MainTokenizer.evaluateInput(tokenizerSvc)

  // You can use this test to debug any input
  property("inputting") {
    evaluateInput("quitter")
  }

  // You can use this test to debug any input
  property("test-factorial") {
    ClinksCalculator.factorial(3) should equal(6)
    ClinksCalculator.factorial(10) should equal(3628800)
  }

  // You can use this test to debug any input
  property("test-calculate-combination") {
    ClinksCalculator.calculateCombination(2, 2) should equal(1)
    ClinksCalculator.calculateCombination(3, 2) should equal(3)
    ClinksCalculator.calculateCombination(4, 2) should equal(6)
    ClinksCalculator.calculateCombination(5, 2) should equal(10)
    ClinksCalculator.calculateCombination(6, 2) should equal(15)
  }

  property("inputting 'quitter'") {
    // capture output for testing therefore it is not shown in the terminal
    val outCapture = new ByteArrayOutputStream
    Console.withOut(outCapture) {
      evaluateInput("quitter") should equal(false)
    }
    outCapture.toString() should include("Adieu.")
  }

  property("inputting 'santé !'") {
    evaluateInput("santé !") should equal(true)
  }
}
