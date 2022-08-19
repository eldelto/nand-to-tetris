package net.eldelto.nand2tetris.jackcompiler

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
      (
        ExpectType[StringIdentifier],
        List(StringIdentifier("test")),
        List(IdentifierNode("test"))
      ),
      (
        Sequence(ExpectToken(Keyword.Var), ExpectType[StringIdentifier]),
        List(Keyword.Var, StringIdentifier("test")),
        List(KeywordNode("var"), IdentifierNode("test"))
      ),
      (
        Repeat(ExpectType[StringIdentifier]),
        List(StringIdentifier("val1"), StringIdentifier("val2")),
        List(IdentifierNode("val1"), IdentifierNode("val2"))
      ),
      (
        Sequence(
          Repeat(ExpectType[StringIdentifier]),
          ExpectToken(Symbol.SemiColon)
        ),
        List(
          StringIdentifier("val1"),
          StringIdentifier("val2"),
          Symbol.SemiColon
        ),
        List(IdentifierNode("val1"), IdentifierNode("val2"), KeywordNode(";"))
      ),
      (
        Or(ExpectType[StringIdentifier], ExpectToken(Keyword.Var)),
        List(Keyword.Var),
        List(KeywordNode("var"))
      ),
      (
        Or(ExpectType[StringIdentifier], ExpectToken(Keyword.Var)),
        List(StringIdentifier("test")),
        List(IdentifierNode("test"))
      ),
      // (VarDec, List(Keyword.Var,Keyword.Int,StringIdentifier("var1"),Symbol.SemiColon), List(VarDecNode(List(KeywordNode("var"), KeywordNode("int"), IdentifierNode("var1"), KeywordNode(";"))))),
      // (
      //   ClassVarDec,
      //   List(Keyword.Static,Keyword.Boolean,StringIdentifier("var1"),Symbol.SemiColon),
      //   List(ClassVarDecNode(List(KeywordNode("static"), KeywordNode("boolean"), IdentifierNode("var1"), KeywordNode(";"))))
      // ),
      // (
      //   Sequence(ClassVarDec, ExpectToken(Symbol.RightCurly)),
      //   List(Keyword.Static,Keyword.Boolean,StringIdentifier("var1"),Symbol.SemiColon, Symbol.RightCurly),
      //   List(ClassVarDecNode(List(KeywordNode("static"), KeywordNode("boolean"), IdentifierNode("var1"), KeywordNode(";"))), KeywordNode("}"))
      // ),
      (
        Sequence(
          Repeat(ExpectType[StringIdentifier]),
          ExpectToken(Symbol.SemiColon)
        ),
        List(StringIdentifier("val1"), Symbol.SemiColon),
        List(IdentifierNode("val1"), KeywordNode(";"))
      ),
      (
        Sequence(
          Repeat(
            Sequence(
              ExpectType[StringIdentifier],
              ExpectToken(Symbol.Ampersand)
            )
          ),
          ExpectToken(Symbol.SemiColon)
        ),
        List(StringIdentifier("val1"), Symbol.Ampersand, Symbol.SemiColon),
        List(IdentifierNode("val1"), KeywordNode("&"), KeywordNode(";"))
      ),
      // (
      //   SubroutineDec,
      //   List(Keyword.Function, Keyword.Void, StringIdentifier("test"), Symbol.LeftParen, Symbol.RightParen, Symbol.LeftCurly, Symbol.RightCurly),
      //   List(SubroutineDecNode(List(KeywordNode("function"), KeywordNode("void"), IdentifierNode("test"), KeywordNode("("), ParameterListNode(List()), KeywordNode(")"), SubroutineBodyNode(List(KeywordNode("{"),StatementsNode(List()), KeywordNode("}"))))))
      // ),
      (
        ReturnStatement,
        List(Keyword.Return, Symbol.SemiColon),
        List(ReturnStatementNode(List(KeywordNode(";"))))
      ),
      (
        Statements,
        List(Keyword.Return, Symbol.SemiColon),
        List(StatementsNode(List(ReturnStatementNode(List(KeywordNode(";"))))))
      ),
      (
        IfStatement,
        List(
          Keyword.If,
          Symbol.LeftParen,
          Keyword.False,
          Symbol.RightParen,
          Symbol.LeftCurly,
          Symbol.RightCurly
        ),
        List(
          IfStatementNode(
            List(
              KeywordNode("if"),
              KeywordNode("("),
              ExpressionNode(List(LiteralTermNode(KeywordNode("false")))),
              KeywordNode(")"),
              KeywordNode("{"),
              StatementsNode(List()),
              KeywordNode("}")
            )
          )
        )
      ),
      (
        Term,
        List(Keyword.True),
        List(LiteralTermNode(KeywordNode("true")))
      ),
      (
        Expression,
        List(StringIdentifier("a"), Symbol.Plus, StringIdentifier("b")),
        List(
          ExpressionNode(
            List(IdentifierTermNode("a"), KeywordNode("+"), IdentifierTermNode("b"))
          )
        )
      ),
      (
        Expression,
        List(Keyword.True),
        List(ExpressionNode(List(LiteralTermNode(KeywordNode("true")))))
      ),
      (
        Sequence(ExpectToken(Keyword.True), Repeat(ExpectToken(Keyword.False))),
        List(Keyword.True),
        List(KeywordNode("true"))
      ),
      (
        ExpressionList,
        List(StringConstant("\"value\"")),
        List(
          ExpressionListNode(
            List(
              ExpressionNode(List(LiteralTermNode(StringConstantNode("value"))))
            )
          )
        )
      ),
      (
        SubroutineCall,
        List(
          StringIdentifier("Object"),
          Symbol.Period,
          StringIdentifier("function"),
          Symbol.LeftParen,
          StringConstant("\"value \""),
          Symbol.RightParen
        ),
        List(
          IdentifierNode("Object"),
          KeywordNode("."),
          IdentifierNode("function"),
          KeywordNode("("),
          ExpressionListNode(
            List(
              ExpressionNode(
                List(LiteralTermNode(StringConstantNode("value ")))
              )
            )
          ),
          KeywordNode(")")
        )
      ),
      (
        Expression,
        List(
          Symbol.LeftParen,
          IntConstant("3"),
          Symbol.Star,
          IntConstant("2"),
          Symbol.RightParen,
          Symbol.Plus,
          IntConstant("1")
        ),
        List(
          ExpressionNode(
            List(
              PriorityTermNode(
                ExpressionNode(
                  List(
                    LiteralTermNode(IntegerConstantNode(3)),
                    KeywordNode("*"),
                    LiteralTermNode(IntegerConstantNode(2))
                  )
                )
              ),
              KeywordNode("+"),
              LiteralTermNode(IntegerConstantNode(1))
            )
          )
        )
      )
    )

    /* Resolving Expressions

      (3*2)+1

      Expression(PriorityTerm(Expression(Term(3), Ops(*), Term(2)))), Ops(+), Term(1))

      For each term call resolveTerm(term) which will handle the priority term first:
      Expression(PriorityTerm(Expression(IntConstant(3), Symbol(*), IntConstant(2)))), Ops(+), Term(1))

      Resolve expression:
      Expression(PriorityTerm(Expression(IntConstant(3), Symbol(*), IntConstant(2)))), Ops(+), Term(1))

      push 3
      push 2
      multiply

      Expression(5, Ops(+), Term(1))

      push 1
      add

     */

    forAll(testData) { (rule, tokens, expected) =>
      val result = rule.execute(new ParserImpl(tokens))
      result shouldBe Right(expected)
    }
  }
}
