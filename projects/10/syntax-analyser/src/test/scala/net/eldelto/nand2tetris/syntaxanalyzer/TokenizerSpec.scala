package net.eldelto.nand2tetris.syntaxanalyzer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TokenizerSpec
    extends AnyFlatSpec
    with Matchers {
  "The tokenizer" should "properly tokenize the given input" in {
    val input = """class MyClass {
      field int x = 0;
    }"""

    val expected = List(
      Keyword.Class,
      Identifier("MyClass"),
    Symbol.LeftCurly,
    Keyword.Field,
    Keyword.Int,
    Identifier("x"),
    Symbol.Equals,
    IntConstant("0"),
    Symbol.SemiColon,
    Symbol.RightCurly,
    )

    val result = tokenize(input.toList)
    result shouldBe expected
    }
  }
