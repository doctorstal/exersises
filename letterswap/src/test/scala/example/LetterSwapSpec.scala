package example

import org.scalatest._

class LetterSwapSpec extends FlatSpec with Matchers {
  "LetterSwap" should "count different letters" in {
    LetterSwap.differentLetters("AAB", "ABC") shouldEqual 2
    LetterSwap.differentLetters("AAB", "AAB") shouldEqual 0
    LetterSwap.differentLetters("DOG", "FOG") shouldEqual 1
    LetterSwap.differentLetters("DOG", "FAR") shouldEqual 3
  }
}
