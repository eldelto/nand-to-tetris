package net.eldelto.nand2tetris.syntaxanalyzer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class ParserSpec
    extends AnyFlatSpec
    with Matchers
    with TableDrivenPropertyChecks {
  "The parser" should "properly parse the given tokens" in {
    val testData = Table[SyntaxRule, List[Token], List[ASTNode]](
      ("rule", "tokens", "AST nodes"),
      (ExpectToken(Keyword.Var), List(Keyword.Var), List()),
      (ExpectType[StringIdentifier], List(StringIdentifier("test")), List(IdentifierNode("test"))),
      (Sequence(ExpectToken(Keyword.Var), ExpectType[StringIdentifier]), List(Keyword.Var, StringIdentifier("test")), List(IdentifierNode("test")))
      // (List(Keyword.Var,StringIdentifier("int"),StringIdentifier("var1"),Symbol.SemiColon), VarDecNode("int", "var1")),
    )

    forAll(testData) { (rule, tokens, expected) =>
      val result = new ParserImpl(tokens).parse(rule)
      result shouldBe Right(expected)
    }
  }
}
