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

  // You can use this to debug Utils.ClinksCalculator.factorial
  property("test-factorial") {
    ClinksCalculator.factorial(3) should equal(6)
    ClinksCalculator.factorial(10) should equal(3628800)
  }

  // You can use this to debug Utils.ClinksCalculator.calculateCombination
  property("test-calculate-combination") {
    ClinksCalculator.calculateCombination(2, 2) should equal(1)
    ClinksCalculator.calculateCombination(3, 2) should equal(3)
    ClinksCalculator.calculateCombination(4, 2) should equal(6)
    ClinksCalculator.calculateCombination(5, 2) should equal(10)
    ClinksCalculator.calculateCombination(6, 2) should equal(15)
  }

  // You can use this to debug Utils.spellCheckerSvc.stringDistance
  property("test-levenshtein") {
    spellCheckerSvc.stringDistance("diète", "diète") should equal(0)
    spellCheckerSvc.stringDistance("bière", "bière") should equal(0)
    spellCheckerSvc.stringDistance("dièto", "bière") should equal(3)
    // identical
    spellCheckerSvc.stringDistance("hello", "hello") should equal(0)
    // same length and differ by one character
    spellCheckerSvc.stringDistance("kitten", "sitten") should equal(1)
    // same length and differ by multiple characters
    spellCheckerSvc.stringDistance("saturday", "sunday") should equal(3)
    // one string is a prefix of the other:
    spellCheckerSvc.stringDistance("stack", "stacks") should equal(1)
    // ne string is empty:
    spellCheckerSvc.stringDistance("hello", "") should equal(5)
    // both strings are empty:
    spellCheckerSvc.stringDistance("", "") should equal(0)
  }

  // You can use this to debug Utils.spellCheckerSvc.getClosestWordInDictionary
  property("test-get-closest-word") {
    // Test case 1: Word is in the dictionary
    spellCheckerSvc.getClosestWordInDictionary("bonjour") should equal(
      "bonjour"
    )
    spellCheckerSvc.getClosestWordInDictionary("hello") should equal(
      "bonjour"
    )
    // Test case 2: Word is misspelled but similar to a word in the dictionary
    spellCheckerSvc.getClosestWordInDictionary("bionjour") should equal(
      "bonjour"
    )
    // Test case 3: Word is misspelled and not similar to any word in the dictionary
    spellCheckerSvc.getClosestWordInDictionary("aloha") should equal("ou")
    // Test case 4: Word is a number
    spellCheckerSvc.getClosestWordInDictionary("123") should equal("123")
    // Test case 5: Word is a pseudonym
    spellCheckerSvc.getClosestWordInDictionary("_sara") should equal("_sara")
    // Test case 6: Word is a single character
    spellCheckerSvc.getClosestWordInDictionary("j") should equal("je")
    // Test case 8: Word is plural
    spellCheckerSvc.getClosestWordInDictionary("croissants") should equal(
      "croissant"
    )
    // Test case 9: Word has accent marks
    spellCheckerSvc.getClosestWordInDictionary("bière") should equal("biere")
    spellCheckerSvc.getClosestWordInDictionary("diète") should equal("biere")
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
