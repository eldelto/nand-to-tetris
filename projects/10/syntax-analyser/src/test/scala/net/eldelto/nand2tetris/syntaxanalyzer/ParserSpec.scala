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
      (ExpectToken(Keyword.Var), List(Keyword.Var), List(KeywordNode("var"))),
      (ExpectType[StringIdentifier], List(StringIdentifier("test")), List(IdentifierNode("test"))),
      (Sequence(ExpectToken(Keyword.Var), ExpectType[StringIdentifier]), List(Keyword.Var, StringIdentifier("test")), List(KeywordNode("var"), IdentifierNode("test"))),
      (Repeat(ExpectType[StringIdentifier]), List(StringIdentifier("val1"), StringIdentifier("val2")), List(IdentifierNode("val1"), IdentifierNode("val2"))),
      (Sequence(Repeat(ExpectType[StringIdentifier]), ExpectToken(Symbol.SemiColon)), List(StringIdentifier("val1"), StringIdentifier("val2"), Symbol.SemiColon), List(IdentifierNode("val1"), IdentifierNode("val2"), KeywordNode(";"))),
      (Or(ExpectType[StringIdentifier], ExpectToken(Keyword.Var)), List(Keyword.Var), List(KeywordNode("var"))),
      (Or(ExpectType[StringIdentifier], ExpectToken(Keyword.Var)), List(StringIdentifier("test")), List(IdentifierNode("test"))),
      (VarDec, List(Keyword.Var,Keyword.Int,StringIdentifier("var1"),Symbol.SemiColon), List(VarDecNode(List(KeywordNode("var"), KeywordNode("int"), IdentifierNode("var1"), KeywordNode(";"))))),
      (
        ClassVarDec, 
        List(Keyword.Static,Keyword.Boolean,StringIdentifier("var1"),Symbol.SemiColon), 
        List(ClassVarDecNode(List(KeywordNode("static"), KeywordNode("boolean"), IdentifierNode("var1"), KeywordNode(";"))))
      ),
      (
        Sequence(ClassVarDec, ExpectToken(Symbol.RightCurly)), 
        List(Keyword.Static,Keyword.Boolean,StringIdentifier("var1"),Symbol.SemiColon, Symbol.RightCurly), 
        List(ClassVarDecNode(List(KeywordNode("static"), KeywordNode("boolean"), IdentifierNode("var1"), KeywordNode(";"))), KeywordNode("}"))
      ),
      (
        Sequence(Repeat(ExpectType[StringIdentifier]), ExpectToken(Symbol.SemiColon)), 
        List(StringIdentifier("val1"), Symbol.SemiColon), 
        List(IdentifierNode("val1"), KeywordNode(";"))
      ),
      (
        Sequence(Repeat(Sequence(ExpectType[StringIdentifier], ExpectToken(Symbol.Ampersand))), ExpectToken(Symbol.SemiColon)), 
        List(StringIdentifier("val1"), Symbol.Ampersand, Symbol.SemiColon), 
        List(IdentifierNode("val1"), KeywordNode("&"), KeywordNode(";"))
      ),
      (
        SubroutineDec,
        List(Keyword.Function, Keyword.Void, StringIdentifier("test"), Symbol.LeftParen, Symbol.RightParen, Symbol.LeftCurly, Symbol.RightCurly), 
        List(SubroutineDecNode(List(KeywordNode("function"), KeywordNode("void"), IdentifierNode("test"), KeywordNode("("), ParameterListNode(List()), KeywordNode(")"), SubroutineBodyNode(List(KeywordNode("{"),StatementsNode(List()), KeywordNode("}"))))))
      ),
      (
        ReturnStatement,
        List(Keyword.Return, Symbol.SemiColon),
        List(ReturnStatementNode(List(KeywordNode("return"), KeywordNode(";"))))
      ),
      (
        Statements,
        List(Keyword.Return, Symbol.SemiColon),
        List(StatementsNode(List(ReturnStatementNode(List(KeywordNode("return"), KeywordNode(";"))))))
      ),
      (
        IfStatement,
        List(Keyword.If, Symbol.LeftParen, Keyword.False, Symbol.RightParen, Symbol.LeftCurly, Symbol.RightCurly),
        List(IfStatementNode(List(KeywordNode("if"), KeywordNode("("), ExpressionNode(List(TermNode(List(KeywordNode("false"))))), KeywordNode(")"), KeywordNode("{"), StatementsNode(List()), KeywordNode("}"))))
      ),
      (
        Term,
        List(Keyword.True),
        List(TermNode(List(KeywordNode("true"))))
      ),
      (
        Expression,
        List(StringIdentifier("a"), Symbol.Plus, StringIdentifier("b")),
        List(ExpressionNode(List(TermNode(List(IdentifierNode("a"))), KeywordNode("+"), TermNode(List(IdentifierNode("b"))))))
      ),
      (
        Expression,
        List(Keyword.True),
        List(ExpressionNode(List(TermNode(List(KeywordNode("true"))))))
      ),
      (
        Sequence(ExpectToken(Keyword.True), Repeat(ExpectToken(Keyword.False))),
        List(Keyword.True),
        List(KeywordNode("true"))
      ),
      (
        ExpressionList,
        List(StringConstant("\"value\"")),
        List(ExpressionListNode(List(ExpressionNode(List(TermNode(List(StringConstantNode("value"))))))))
      ),
      (
        SubroutineCall,
        List(StringIdentifier("Object"), Symbol.Period, StringIdentifier("function"), Symbol.LeftParen, StringConstant("\"value \""), Symbol.RightParen),
        List(IdentifierNode("Object"), KeywordNode("."), IdentifierNode("function"), KeywordNode("("), ExpressionListNode(List(ExpressionNode(List(TermNode(List(StringConstantNode("value "))))))), KeywordNode(")"))
      ),
    )

    forAll(testData) { (rule, tokens, expected) =>
      println("before")
      val result = rule.execute(new ParserImpl(tokens))
      println("after" + result)
      result shouldBe Right(expected)
    }
  }
}
