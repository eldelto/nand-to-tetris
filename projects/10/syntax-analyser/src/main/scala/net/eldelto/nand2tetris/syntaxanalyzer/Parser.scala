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
case class IdentifierNode(value: String) extends ASTNode
case class VarDecNode(dataType: String, identifier: String) extends ASTNode

trait SyntaxRule {
  def execute(parser: Parser): Either[Throwable, List[ASTNode]]
}

// type SyntaxRule = (parser: Parser) => Either[Throwable, List[ASTNode]]

val Identifier = ExpectType[StringIdentifier]
// val VarDec = Sequence(
//   ExpectToken(Keyword.Var),
//   Identifier,
//   Identifier,
//   ExpectToken(Symbol.SemiColon)
// )

class VarDec extends SyntaxRule {
  private val rule = Sequence(
    ExpectToken(Keyword.Var),
    Identifier, // TODO: Or
    Identifier,
    ExpectToken(Symbol.SemiColon)
  )
  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      val varType = nodes(0).asInstanceOf[IdentifierNode].value
      val name = nodes(1).asInstanceOf[IdentifierNode].value
      VarDecNode(varType, name).pure[List]
    }
  }
}

class Sequence(val rules: SyntaxRule*) extends SyntaxRule {
  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] =
    val acc = rules.head.execute(parser)
    rules.tail.foldLeft(acc) { (acc, rule) =>
      for {
        _ <- parser.advance()
        accNodes <- acc
        nodes <- rule.execute(parser)
      } yield accNodes ++ nodes
    }
}

class Repeat(val rule: SyntaxRule) extends SyntaxRule {
  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    var result = List[ASTNode]().asRight[Throwable]
    var advancable = true
    while (advancable) {
      result = for {
        resultNodes <- result
        nodes <- rule.execute(parser)
      } yield resultNodes ++ nodes
      advancable = parser.advance().isRight
    }

    result
  }
}

class Or(val rules: SyntaxRule*) extends SyntaxRule {
  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = rules
    .map(_.execute(parser))
    .find(_.isRight)
    .getOrElse(new IllegalArgumentException("No or clause matched").asLeft)
}

object ExpectType {
  def apply[T <: Token](using tt: TypeTest[Token, T]): SyntaxRule = {
    (parser: Parser) =>
      parser.getToken() match {
        case token: T =>
          IdentifierNode(token.value).pure[List].asRight[Throwable]
        case token =>
          new IllegalArgumentException("Unexpected token type: " + token)
            .asLeft[List[ASTNode]]
      }
  }
}

class ExpectToken[T <: Token](val expected: T) extends SyntaxRule {
  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] =
    if (parser.getToken() == expected) {
      List[ASTNode]().asRight[Throwable]
    } else {
      new IllegalArgumentException("Unexpected token: " + parser.getToken())
        .asLeft[List[ASTNode]]
    }
}

// val varDecRules = Keyword.Var :: expectType[VarType] :: expectType[Identifier] :: Multiple(Symbol.Comma :: expectedType[Identifier])

trait Parser {
  def getToken(): Token
  def getNextToken(): Option[Token]
  def advance(): Either[Throwable, Unit]
}

class ParserImpl(val tokens: List[Token]) extends Parser {
  private val tokenIterator = tokens.iterator
  private var token: Token = tokenIterator.next
  private var nextToken: Option[Token] = tokenIterator.nextOption

  override def getToken(): Token = token

  override def getNextToken(): Option[Token] = nextToken

  override def advance(): Either[Throwable, Unit] = {
    if (nextToken.isEmpty) {
      new IllegalStateException("No tokens left").asLeft
    } else if (!tokenIterator.hasNext) {
      token = nextToken.get
      nextToken = None
      ().asRight
    } else {
      token = nextToken.get
      nextToken = tokenIterator.nextOption
      ().asRight
    }
  }
}
