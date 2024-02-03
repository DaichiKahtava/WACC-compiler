package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.lexer

class lexerTests extends AnyFlatSpec {
  "A lexer" should "produce a Failure for an empty input" in {
    val input = ""
    lexer.ident.parse(input) match {
      case parsley.Failure(_) => succeed
      case _ => fail("Expected a Failure, but parsing succeeded.")
    }
  }

  it should "correctly tokenize an identifier" in {
    val input = "identifier"
    val expectedOutput = "identifier"
    lexer.ident.parse(input).get shouldBe expectedOutput
  }

  it should "correctly tokenize an integer literal" in {
    val input = "12345"
    val expectedOutput = 12345
    lexer.intLit.parse(input).get shouldBe (expectedOutput)
  }

  it should "correctly tokenize a character literal" in {
    val input = "'a'"
    val expectedOutput = 'a'
    lexer.charLit.parse(input).get shouldBe (expectedOutput)
  }

  it should "correctly tokenize a string literal" in {
    val input = "\"hello world\""
    val expectedOutput = "hello world"
    lexer.strLit.parse(input).get shouldBe (expectedOutput)
  }

  it should "reject a keyword for an identifier" in {
    val input = "begin"
    val expectedOutput = "begin"
    lexer.ident.parse(input).isFailure shouldBe true
  }

  it should "fail to tokenize a hard operator as an identifier" in {
    val input = "&&"
    lexer.ident.parse(input) match {
      case parsley.Failure(_) => succeed
      case _ => fail("Expected a Failure, but parsing succeeded.")
    }
  }

  it should "fail to tokenize an invalid identifier" in {
    val input = "123invalid"
    lexer.ident.parse(input) match {
      case parsley.Failure(_) => succeed
      case _ => fail("Expected a Failure, but parsing succeeded.")
    }
  }

  it should "correctly tokenize a string with escape sequences" in {
    val input = "\"hello\\nworld\""
    val expectedOutput = "hello\nworld"
    lexer.strLit.parse(input).get shouldBe expectedOutput
  }

  it should "fail to tokenize a string with invalid escape sequences" in {
    val input = "\"hello\\xworld\""
    lexer.strLit.parse(input) match {
      case parsley.Failure(_) => succeed
      case _ => fail("Expected a Failure, but parsing succeeded.")
    }
  }

  it should "parse an escaped \"\\\" " in {
    val input = "\"hello\\\\world\""
    val expectedOutput = "hello\\world"
    lexer.strLit.parse(input).get shouldBe (expectedOutput)
  }

}
