package Chat

import Chat.Token.*
import Utils.SpellCheckerService
//import Chat.Tokenized.nextToken

trait Tokenized:
  /** Get the next token of the user input, or EOL if there is no more token.
    * @return
    *   a tuple that contains the string value of the current token, and the
    *   identifier of the token
    */
  def nextToken(): (String, Token)

class TokenizedImpl(val tokens: Array[(String, Token)]) extends Tokenized:
  // TODO - Part 1 Step 3
  private var index = 0
  def nextToken(): (String, Token) =
    if index < tokens.length then { index += 1; tokens(index - 1) }
    else { index = 0; ("EOL", Token.EOL) }
  end nextToken

end TokenizedImpl
