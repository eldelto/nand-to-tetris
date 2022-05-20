package net.eldelto.nand2tetris.syntaxanalyzer

import scala.reflect.TypeTest
import cats.data.EitherT
import cats.implicits._
import scala.collection.mutable.Stack
import java.util.Queue
import java.util.concurrent.ArrayBlockingQueue
import org.apache.commons.collections4.queue.CircularFifoQueue

// sealed trait SyntaxNode
// case class Or(nodes: SyntaxNode*)
// case class Multiple(nodes: SyntaxNode*)

// sealed trait VarType extends Token
// case class VarDeclaration(varType: VarType, identifier: StringIdentifier)
//     extends SyntaxNode

sealed trait ASTNode
case class IdentifierNode(value: String) extends ASTNode
case class KeywordNode(value: String) extends ASTNode
case class VarDecNode(children: List[ASTNode]) extends ASTNode

trait SyntaxRule {
  def execute(parser: Parser): Either[Throwable, List[ASTNode]]
}

// type SyntaxRule = (parser: Parser) => Either[Throwable, List[ASTNode]]

val Identifier = ExpectType[StringIdentifier]
val TypeInt = ExpectToken(Keyword.Int)
val TypeChar = ExpectToken(Keyword.Char)
val TypeBoolean = ExpectToken(Keyword.Boolean)
val Type = Or(TypeInt, TypeChar, TypeBoolean, Identifier)

class VarDec extends SyntaxRule {
  private val rule = Sequence(
    ExpectToken(Keyword.Var),
    Type, 
    Identifier,
    Repeat(
      Sequence(
        ExpectToken(Symbol.Comma),
        Identifier
      )
    ),
    ExpectToken(Symbol.SemiColon)
  )
  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      VarDecNode(nodes).pure[List]
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
    val token = parser.getToken()
    if (token == expected) {
      KeywordNode(token.value).pure[List].asRight[Throwable]
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
  def rewind(count: Int): Unit
}

class ParserImpl(val tokens: List[Token]) extends Parser {
  private val tokenIterator = tokens.iterator
  private val tokenBuffer: RingBuffer[Token] = new RingBuffer(10)
  private var bufferIndex: Int = 0

  {
    this.advance()
    this.advance()
  }

  override def getToken(): Token = tokenBuffer.get(bufferIndex + 1).get

  override def getNextToken(): Option[Token] = if (bufferIndex >= 0) tokenBuffer.get(bufferIndex) else Option.empty

  override def advance(): Either[Throwable, Unit] = {
    if (bufferIndex > 0) {
      bufferIndex -= 1
      ().asRight
    } else if (bufferIndex <= -1) {
      new IllegalStateException("No tokens left").asLeft
    } else if (!tokenIterator.hasNext) {
      bufferIndex -= 1
      ().asRight
    } else {
      tokenBuffer.append(tokenIterator.next)
      ().asRight
    }
  }
  
  override def rewind(count: Int): Unit = {
    if (count >= tokenBuffer.size) throw IllegalArgumentException("Rewind count is larger than buffer size: " + tokenBuffer.size)
    bufferIndex = count
  }
}
