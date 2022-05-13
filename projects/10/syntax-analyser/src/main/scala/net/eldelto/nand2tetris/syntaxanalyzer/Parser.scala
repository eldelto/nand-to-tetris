package net.eldelto.nand2tetris.syntaxanalyzer

import scala.reflect.TypeTest
import cats.data.EitherT
import cats.implicits._

// sealed trait SyntaxNode
// case class Or(nodes: SyntaxNode*)
// case class Multiple(nodes: SyntaxNode*)

// sealed trait VarType extends Token
// case class VarDeclaration(varType: VarType, identifier: StringIdentifier)
//     extends SyntaxNode

sealed trait ASTNode
case class DummyASTNode() extends ASTNode
case class IdentifierNode(value: String) extends ASTNode
case class VarDecNode(dataType: String, identifier: String) extends ASTNode

// trait SyntaxRule {
//   def execute(
//       token: Token,
//       nextToken: Option[Token],
//       parser: Parser
//   ): Either[Throwable, List[ASTNode]]
// }

type SyntaxRule = (parser: Parser) => Either[Throwable, List[ASTNode]]

val Identifier = ExpectType[StringIdentifier]
val VarDec = Sequence(
  ExpectToken(Keyword.Var),
  ExpectType[StringIdentifier],
  ExpectType[StringIdentifier],
  ExpectToken(Symbol.SemiColon)
)

object Sequence {
  def apply(rules: SyntaxRule*): SyntaxRule = {
    (parser: Parser) =>
      rules.foldLeft(List[ASTNode]().asRight[Throwable]) { (acc, rule) =>
        for {
          accNodes <- acc
          nodes <- rule(parser)
        } yield accNodes ++ nodes
      }
  }
}

object ExpectType {
  def apply[T <: Token](using tt: TypeTest[Token, T]): SyntaxRule = {
    (parser: Parser) =>
      parser.getToken() match {
        case token: T =>
          parser.advance()
          IdentifierNode(token.value).pure[List].asRight[Throwable]
        case token =>
          new IllegalArgumentException("Unexpected token type: " + token)
            .asLeft[List[ASTNode]]
      }
  }
}

object ExpectToken {
  def apply[T <: Token](expected: T): SyntaxRule = {
    (parser: Parser) =>
      if (parser.getToken() == expected) {
        parser.advance()
        List[ASTNode]().asRight[Throwable]
      } else {
        new IllegalArgumentException("Unexpected token: " + parser.getToken())
          .asLeft[List[ASTNode]]
      }
  }
}

// val varDecRules = Keyword.Var :: expectType[VarType] :: expectType[Identifier] :: Multiple(Symbol.Comma :: expectedType[Identifier])

trait Parser {
  def getToken(): Token
  def getNextToken(): Option[Token]
  def advance(): Boolean
}

class ParserImpl(val tokens: List[Token]) extends Parser {
  private val tokenIterator = tokens.iterator
  private var token: Token = tokenIterator.next
  private var nextToken: Option[Token] = tokenIterator.nextOption

  override def getToken(): Token = token

  override def getNextToken(): Option[Token] = nextToken

  override def advance(): Boolean = {
    println("in: " + token)
    if (nextToken.isEmpty) {
    println("out: " + token)
      false
    } else if (!tokenIterator.hasNext) {
      token = nextToken.get
      nextToken = None
    println("out: " + token)
      false
    } else {
      token = nextToken.get
      nextToken = tokenIterator.nextOption
    println("out: " + token)
      true
    }
  }

  def parse(rule: SyntaxRule): Either[Throwable, List[ASTNode]] =
    rule(this)
}
