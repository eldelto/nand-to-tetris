package net.eldelto.nand2tetris.jackcompiler

import scala.reflect.TypeTest
import cats.data.EitherT
import cats.implicits._
import scala.collection.mutable.Stack
import java.util.Queue
import java.util.concurrent.ArrayBlockingQueue

enum VariableType {
  case Local
  case Field
  case Static
}

enum SubroutineType {
  case Constructor
  case Function
  case Method
}

case class SingleVariableDec(name: String, valueType: String, variableType: VariableType)

sealed trait ASTNode
case class IdentifierNode(value: String) extends ASTNode
case class IntegerConstantNode(value: Int) extends ASTNode
case class StringConstantNode(value: String) extends ASTNode
case class KeywordNode(value: String) extends ASTNode
case class ClassNode(name: String, children: List[ASTNode]) extends ASTNode
case class ClassVarDecNode(declarations: List[SingleVariableDec], children: List[ASTNode]) extends ASTNode
case class VarDecNode(declarations: List[SingleVariableDec], children: List[ASTNode]) extends ASTNode
case class SubroutineDecNode(routineType: SubroutineType, name: String, returnType: String, parameters: ParameterListNode, children: List[ASTNode]) extends ASTNode
case class ParameterListNode(children: List[ASTNode]) extends ASTNode
case class SubroutineBodyNode(children: List[ASTNode]) extends ASTNode
case class StatementsNode(children: List[ASTNode]) extends ASTNode
case class LetStatementNode(children: List[ASTNode]) extends ASTNode
case class IfStatementNode(children: List[ASTNode]) extends ASTNode
case class WhileStatementNode(children: List[ASTNode]) extends ASTNode
case class DoStatementNode(children: List[ASTNode]) extends ASTNode
case class ReturnStatementNode(children: List[ASTNode]) extends ASTNode
case class ExpressionNode(children: List[ASTNode]) extends ASTNode
case class TermNode(children: List[ASTNode]) extends ASTNode
case class ExpressionListNode(children: List[ASTNode]) extends ASTNode

trait Parser {
  def getToken(): Token
  def getNextToken(): Option[Token]
  def advance(): Either[Throwable, Unit]
  def rewind(count: Int): Unit
}

class ParserImpl(val tokens: List[Token]) extends Parser {
  private val tokenIterator = tokens.iterator
  private val tokenBuffer: RingBuffer[Token] = new RingBuffer(100)
  private var bufferIndex: Int = 0

  {
    this.advance()
    this.advance()
  }

  override def getToken(): Token = {
    val token = tokenBuffer.get(bufferIndex + 1)
    // println(s"getting to $token index=$bufferIndex")
    if (token.isEmpty) {
      throw new IllegalStateException("No token exists at index " + bufferIndex)
    }
    token.get
  }

  override def getNextToken(): Option[Token] =
    if (bufferIndex >= 0) tokenBuffer.get(bufferIndex) else Option.empty

  override def advance(): Either[Throwable, Unit] = {
    if (bufferIndex > 0) {
      bufferIndex -= 1
      ().asRight
    } else if (bufferIndex <= -1) {
      new IllegalStateException("No tokens left").asLeft
    } else if (!tokenIterator.hasNext) {
      bufferIndex = -1
      ().asRight
    } else {
      tokenBuffer.append(tokenIterator.next)
      ().asRight
    }
  }

  override def rewind(count: Int): Unit = {
    if (count >= tokenBuffer.size)
      throw IllegalArgumentException(
        "Rewind count is larger than buffer size: " + tokenBuffer.size
      )
    bufferIndex += count
    // println(s"rewinding to index=$bufferIndex")
  }
}

trait SyntaxRule {
  def execute(parser: Parser): Either[Throwable, List[ASTNode]]
}

class Sequence(val rules: SyntaxRule*) extends SyntaxRule {
  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    var result = rules.head.execute(parser)
    if (result.isLeft) {
      return result
    }

    var i = 1
    for (rule <- rules.tail) {
      i += 1

      // Hack to handle optional tokens at the end of a Sequence.
      if (parser.advance().isLeft) {
        return result
      }

      result = for {
        resultnodes <- result
        nodes <- rule.execute(parser)
      } yield resultnodes ++ nodes

      if (result.isLeft) {
        parser.rewind(i - 1)
        return result
      }
    }

    result
  }
}

class Repeat(val rule: SyntaxRule) extends SyntaxRule {
  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    var result = List[ASTNode]().asRight[Throwable]
    var advancable = true
    var i = 0
    while (advancable) {
      i += 1
      val tmpResult = for {
        resultNodes <- result
        nodes <- rule.execute(parser)
      } yield resultNodes ++ nodes

      if (tmpResult.isRight) result = tmpResult
      else parser.rewind(1)

      advancable = tmpResult.isRight && parser.advance().isRight
    }

    result match {
      case Right(nodes) if nodes.size == 0 =>
        // parser.rewind(i)
        result
      case Right(_) => result
      case Left(_)  =>
        // parser.rewind(i-1)
        List[ASTNode]().asRight[Throwable]
    }
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
          tokenToAST(token).pure[List].asRight[Throwable]
        case token =>
          new IllegalArgumentException(
            s"Unexpected token type: Wanted $tt but got $token"
          )
            .asLeft[List[ASTNode]]
      }
  }

  private def tokenToAST(token: Token): ASTNode = {
    val value = token.value
    value.toIntOption
    .map[ASTNode](IntegerConstantNode(_))
    .orElse({
      if (value.startsWith("\"")) Some(StringConstantNode(value.substring(1, value.length - 1)))
      else None
    })
    .getOrElse(IdentifierNode(value))
  }
}

class ExpectToken[T <: Token](val expected: T) extends SyntaxRule {
  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] =
    val token = parser.getToken()
    if (token == expected) {
      KeywordNode(token.value).pure[List].asRight[Throwable]
    } else {
      new IllegalArgumentException(
        s"Unexpected token: Wanted $expected but got $token"
      )
        .asLeft[List[ASTNode]]
    }
}

lazy val Identifier = ExpectType[StringIdentifier]
lazy val TypeInt = ExpectToken(Keyword.Int)
lazy val TypeChar = ExpectToken(Keyword.Char)
lazy val TypeBoolean = ExpectToken(Keyword.Boolean)
lazy val Type = Or(TypeInt, TypeChar, TypeBoolean, Identifier)

object Class extends SyntaxRule {
  private val rule = Sequence(
    ExpectToken(Keyword.Class),
    Identifier,
    ExpectToken(Symbol.LeftCurly),
    Repeat(ClassVarDec),
    Repeat(SubroutineDec),
    ExpectToken(Symbol.RightCurly)
  )
  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      val name = nodes(1).asInstanceOf[IdentifierNode].value
      ClassNode(name, nodes).pure[List]
    }
  }
}

object ClassVarDec extends SyntaxRule {
  private val rule = Sequence(
    Or(
      ExpectToken(Keyword.Static),
      ExpectToken(Keyword.Field)
    ),
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
      val variableTypeValue = nodes(0).asInstanceOf[KeywordNode].value
      val variableType = variableTypeValue match {
        case Keyword.Static.value => VariableType.Static
        case Keyword.Field.value => VariableType.Field
      }

      val valueType = nodes(1) match {
        case n: IdentifierNode => n.value 
        case n: KeywordNode => n.value
        case _ => throw new IllegalStateException("Unexpected node")
      }

      // TODO: Handle multiple variable declarations in one statement.
      val name = nodes(2).asInstanceOf[IdentifierNode].value
      val declarations = List(SingleVariableDec(name, valueType, variableType))

      ClassVarDecNode(declarations, nodes).pure[List]
    }
  }
}

object SubroutineDec extends SyntaxRule {
  private val rule = Sequence(
    Or(
      ExpectToken(Keyword.Constructor),
      ExpectToken(Keyword.Function),
      ExpectToken(Keyword.Method)
    ),
    Or(
      ExpectToken(Keyword.Void),
      Type
    ),
    Identifier,
    ExpectToken(Symbol.LeftParen),
    ParameterList,
    ExpectToken(Symbol.RightParen),
    SubroutineBody
  )

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      val typeValue = nodes(0) match {
        case n: IdentifierNode => n.value
        case n: KeywordNode => n.value
        case _ => throw new IllegalStateException("Unexpected node")
      }
      val routineType = typeValue match {
        case Keyword.Constructor.value => SubroutineType.Constructor
        case Keyword.Function.value => SubroutineType.Function
        case Keyword.Method.value => SubroutineType.Method
      }

      val returnType = nodes(1) match {
        case n: IdentifierNode => n.value
        case n: KeywordNode => n.value
        case _ => throw new IllegalStateException("Unexpected node")
      }

      val name = nodes(2).asInstanceOf[IdentifierNode].value
      val parameterList = nodes(4).asInstanceOf[ParameterListNode]

      SubroutineDecNode(routineType, name, returnType, parameterList, nodes).pure[List]
    }
  }
}

object ParameterList extends SyntaxRule {
  private val rule = Repeat(
    Sequence( // TODO: Implement Optional(...)
      Type,
      Identifier,
      Repeat(
        Sequence(
          ExpectToken(Symbol.Comma),
          Type,
          Identifier
        )
      )
    )
  )

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      ParameterListNode(nodes).pure[List]
    }
  }
}

object SubroutineBody extends SyntaxRule {
  private val rule = Sequence(
    ExpectToken(Symbol.LeftCurly),
    Repeat(VarDec),
    Statements,
    ExpectToken(Symbol.RightCurly)
  )

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      SubroutineBodyNode(nodes).pure[List]
    }
  }
}

object VarDec extends SyntaxRule {
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
      val variableTypeValue = nodes(0).asInstanceOf[KeywordNode].value
      val variableType = variableTypeValue match {
        case Keyword.Var.value => VariableType.Local
      }

      val valueType = nodes(1) match {
        case n: IdentifierNode => n.value 
        case n: KeywordNode => n.value
        case _ => throw new IllegalStateException("Unexpected node")
      }

      // TODO: Handle multiple variable declarations in one statement.
      val name = nodes(2).asInstanceOf[IdentifierNode].value
      val declarations = List(SingleVariableDec(name, valueType, variableType))

      VarDecNode(declarations, nodes).pure[List]
    }
  }
}

object Statements extends SyntaxRule {
  private val rule = Repeat(
    Or(
      LetStatement,
      IfStatement,
      WhileStatement,
      DoStatement,
      ReturnStatement
    )
  )

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      StatementsNode(nodes).pure[List]
    }
  }
}

object LetStatement extends SyntaxRule {
  private val rule = Sequence(
    ExpectToken(Keyword.Let),
    Identifier,
    Repeat( // TODO: Optional
      Sequence(
        ExpectToken(Symbol.LeftBracket),
        Expression,
        ExpectToken(Symbol.RightBracket)
      )
    ),
    ExpectToken(Symbol.Equals),
    Expression,
    ExpectToken(Symbol.SemiColon)
  )

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      LetStatementNode(nodes).pure[List]
    }
  }
}

object IfStatement extends SyntaxRule {
  private val rule = Sequence(
    ExpectToken(Keyword.If),
    ExpectToken(Symbol.LeftParen),
    Expression,
    ExpectToken(Symbol.RightParen),
    ExpectToken(Symbol.LeftCurly),
    Statements,
    ExpectToken(Symbol.RightCurly),
    Repeat( // TODO: Optional
      Sequence(
        ExpectToken(Keyword.Else),
        ExpectToken(Symbol.LeftCurly),
        Statements,
        ExpectToken(Symbol.RightCurly)
      )
    )
  )

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      IfStatementNode(nodes).pure[List]
    }
  }
}

object WhileStatement extends SyntaxRule {
  private val rule = Sequence(
    ExpectToken(Keyword.While),
    ExpectToken(Symbol.LeftParen),
    Expression,
    ExpectToken(Symbol.RightParen),
    ExpectToken(Symbol.LeftCurly),
    Statements,
    ExpectToken(Symbol.RightCurly)
  )

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      WhileStatementNode(nodes).pure[List]
    }
  }
}

object DoStatement extends SyntaxRule {
  private val rule = Sequence(
    ExpectToken(Keyword.Do),
    SubroutineCall,
    ExpectToken(Symbol.SemiColon)
  )

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      DoStatementNode(nodes).pure[List]
    }
  }
}

object ReturnStatement extends SyntaxRule {
  private val rule = Sequence(
    ExpectToken(Keyword.Return),
    Repeat( // TODO: Optional
      Expression
    ),
    ExpectToken(Symbol.SemiColon)
  )

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      ReturnStatementNode(nodes).pure[List]
    }
  }
}

object Expression extends SyntaxRule {
  private val rule = Sequence(
    Term,
    Repeat(
      Sequence(
        Op,
        Term
      )
    )
  )

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      ExpressionNode(nodes).pure[List]
    }
  }
}

object Term extends SyntaxRule {
  private val rule = Or(
    SubroutineCall,
    Sequence(
      Identifier,
      ExpectToken(Symbol.LeftBracket),
      Expression,
      ExpectToken(Symbol.RightBracket)
    ),
    Sequence(
      ExpectToken(Symbol.LeftParen),
      Expression,
      ExpectToken(Symbol.RightParen)
    ),
    ExpectType[IntConstant],
    ExpectType[StringConstant],
    KeywordConstant,
    Identifier,
    Sequence(
      UnaryOp,
      Term
    )
  )

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      TermNode(nodes).pure[List]
    }
  }
}

lazy val SubroutineCall = Or(
  Sequence(
    Identifier,
    ExpectToken(Symbol.LeftParen),
    ExpressionList,
    ExpectToken(Symbol.RightParen)
  ),
  Sequence(
    Identifier,
    ExpectToken(Symbol.Period),
    Identifier,
    ExpectToken(Symbol.LeftParen),
    ExpressionList,
    ExpectToken(Symbol.RightParen)
  )
)

object ExpressionList extends SyntaxRule {
  private val rule = Repeat( // TODO: Optional
    Sequence(
      Expression,
      Repeat(
        Sequence(
          ExpectToken(Symbol.Comma),
          Expression
        )
      )
    )
  )

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      ExpressionListNode(nodes).pure[List]
    }
  }
}

lazy val Op = Or(
  ExpectToken(Symbol.Plus),
  ExpectToken(Symbol.Minus),
  ExpectToken(Symbol.Star),
  ExpectToken(Symbol.Slash),
  ExpectToken(Symbol.Ampersand),
  ExpectToken(Symbol.Pipe),
  ExpectToken(Symbol.LessThan),
  ExpectToken(Symbol.GreaterThan),
  ExpectToken(Symbol.Equals)
)

lazy val UnaryOp = Or(
  ExpectToken(Symbol.Minus),
  ExpectToken(Symbol.Tilde)
)

lazy val KeywordConstant = Or(
  ExpectToken(Keyword.True),
  ExpectToken(Keyword.False),
  ExpectToken(Keyword.Null),
  ExpectToken(Keyword.This)
)
