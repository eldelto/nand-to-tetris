package net.eldelto.nand2tetris.jackcompiler

import scala.reflect.TypeTest
import cats.data.EitherT
import cats.implicits.*
import scala.collection.mutable.Stack
import java.util.Queue
import java.util.concurrent.ArrayBlockingQueue
import scala.util.control.Breaks.*

enum VariableType {
  case Local
  case Field
  case Static
  case Argument
}

enum SubroutineType {
  case Constructor
  case Function
  case Method
}

case class SingleVariableDec(
    name: String,
    valueType: String,
    variableType: VariableType
)

sealed trait ASTNode
case class IdentifierNode(value: String) extends ASTNode

trait LiteralNode extends ASTNode
case class IntegerConstantNode(value: Int) extends LiteralNode
case class StringConstantNode(value: String) extends LiteralNode
case class KeywordNode(value: String) extends LiteralNode

case class ClassNode(name: String, children: List[ASTNode]) extends ASTNode
case class ClassVarDecNode(
    declarations: List[SingleVariableDec],
    children: List[ASTNode]
) extends ASTNode
case class VarDecNode(declarations: List[SingleVariableDec]) extends ASTNode
case class SubroutineDecNode(
    routineType: SubroutineType,
    name: String,
    returnType: String,
    parameters: ParameterListNode,
    body: SubroutineBodyNode
) extends ASTNode
case class ParameterListNode(variables: List[SingleVariableDec]) extends ASTNode
case class SubroutineBodyNode(children: List[ASTNode]) extends ASTNode
case class StatementsNode(children: List[ASTNode]) extends ASTNode
case class LetStatementNode(variableName: String, children: List[ASTNode])
    extends ASTNode
case class ArrayLetStatementNode(
    variableName: String,
    indexExpression: ExpressionNode,
    expression: ExpressionNode
) extends ASTNode
case class IfStatementNode(
    condition: ExpressionNode,
    body: StatementsNode,
    elseBody: Option[StatementsNode]
) extends ASTNode
case class WhileStatementNode(condition: ExpressionNode, body: StatementsNode)
    extends ASTNode
case class SubroutineCallNode(
    calleeName: String,
    parameters: ExpressionListNode,
    children: List[ASTNode]
) extends ASTNode
case class DoStatementNode(callee: SubroutineCallNode) extends ASTNode
case class ReturnStatementNode(children: List[ASTNode]) extends ASTNode
case class ExpressionNode(children: List[ASTNode]) extends ASTNode
case class ExpressionListNode(children: List[ASTNode]) extends ASTNode

trait TermNode extends ASTNode
case class PriorityTermNode(expression: ExpressionNode) extends TermNode
case class LiteralTermNode(literal: LiteralNode) extends TermNode
case class IdentifierTermNode(identifier: String) extends TermNode
case class ArrayIdentifierTermNode(identifier: String, index: ExpressionNode)
    extends TermNode
case class GenericTermNode(children: List[ASTNode]) extends TermNode

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
        if (value.startsWith("\""))
          Some(StringConstantNode(value.substring(1, value.length - 1)))
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
        case Keyword.Field.value  => VariableType.Field
      }

      val valueType = nodes(1) match {
        case n: IdentifierNode => n.value
        case n: KeywordNode    => n.value
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
        case n: KeywordNode    => n.value
        case _ => throw new IllegalStateException("Unexpected node")
      }
      val routineType = typeValue match {
        case Keyword.Constructor.value => SubroutineType.Constructor
        case Keyword.Function.value    => SubroutineType.Function
        case Keyword.Method.value      => SubroutineType.Method
      }

      val returnType = nodes(1) match {
        case n: IdentifierNode => n.value
        case n: KeywordNode    => n.value
        case _ => throw new IllegalStateException("Unexpected node")
      }

      val name = nodes(2).asInstanceOf[IdentifierNode].value
      val parameterList = nodes(4).asInstanceOf[ParameterListNode]
      val body = nodes(6).asInstanceOf[SubroutineBodyNode]

      SubroutineDecNode(routineType, name, returnType, parameterList, body)
        .pure[List]
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
      var declarations: List[SingleVariableDec] = List()
      if (nodes.length >= 2) {
        var i = 0
        breakable {
          for (node <- nodes) {
            val valueType = nodes(i) match {
              case n: IdentifierNode => n.value
              case n: KeywordNode    => n.value
              case n =>
                throw new IllegalStateException(
                  s"Expected variable type but got: $n"
                )
            }

            val name = nodes(i + 1).asInstanceOf[IdentifierNode].value
            declarations = declarations
              .appended(
                SingleVariableDec(name, valueType, VariableType.Argument)
              )

            i = i + 3
            if (nodes.length <= i)
              break
          }
        }
      }

      ParameterListNode(declarations).pure[List]
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
        case n: KeywordNode    => n.value
        case _ => throw new IllegalStateException("Unexpected node")
      }

      var declarations: List[SingleVariableDec] = List()
      var i = 2
      breakable {
        for (node <- nodes) {
          val name = nodes(i).asInstanceOf[IdentifierNode].value
          declarations = declarations
            .appended(SingleVariableDec(name, valueType, variableType))
          if (nodes(i + 1) == KeywordNode(";"))
            break
          i = i + 2
        }
      }

      VarDecNode(declarations).pure[List]
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
      val variableName = nodes(1).asInstanceOf[IdentifierNode]

      nodes(2) match {
        case KeywordNode("=") =>
          LetStatementNode(variableName.value, nodes.tail.tail).pure[List]
        case KeywordNode("[") =>
          val indexExpression = nodes(3).asInstanceOf[ExpressionNode]
          val expression = nodes(6).asInstanceOf[ExpressionNode]
          ArrayLetStatementNode(variableName.value, indexExpression, expression)
            .pure[List]
        case n =>
          throw IllegalStateException(s"Unexpected node type: ${n.getClass}")
      }
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
      val condition = nodes(2).asInstanceOf[ExpressionNode]
      val body = nodes(5).asInstanceOf[StatementsNode]
      val elseBody =
        if (nodes.length > 9) Some(nodes(9).asInstanceOf[StatementsNode])
        else None

      IfStatementNode(condition, body, elseBody).pure[List]
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
      val condition = nodes(2).asInstanceOf[ExpressionNode]
      val body = nodes(5).asInstanceOf[StatementsNode]
      WhileStatementNode(condition, body).pure[List]
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
      val callee = nodes(1).asInstanceOf[SubroutineCallNode]
      DoStatementNode(callee).pure[List]
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
      ReturnStatementNode(nodes.tail).pure[List]
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
    // Array
    Sequence(
      Identifier,
      ExpectToken(Symbol.LeftBracket),
      Expression,
      ExpectToken(Symbol.RightBracket)
    ),
    // Unary term
    Identifier,
    Sequence(
      UnaryOp,
      Term
    ),
    // Priority term
    Sequence(
      ExpectToken(Symbol.LeftParen),
      Expression,
      ExpectToken(Symbol.RightParen)
    ),
    // Literals
    ExpectType[IntConstant],
    ExpectType[StringConstant],
    KeywordConstant
  )

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      val result = nodes(0) match {
        case KeywordNode("(") =>
          PriorityTermNode(nodes(1).asInstanceOf[ExpressionNode])
        case n: (IntegerConstantNode | StringConstantNode | KeywordNode) =>
          LiteralTermNode(n)
        case n: IdentifierNode
            if (nodes.length > 1 && nodes(1) == KeywordNode("[")) =>
          val indexExpression = nodes(2).asInstanceOf[ExpressionNode]
          ArrayIdentifierTermNode(n.value, indexExpression)
        case n: IdentifierNode => IdentifierTermNode(n.value)
        case _                 => GenericTermNode(nodes)
      }

      result.pure[List]
    }
  }
}

object SubroutineCall extends SyntaxRule {
  private val rule = Or(
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

  override def execute(parser: Parser): Either[Throwable, List[ASTNode]] = {
    rule.execute(parser).map { nodes =>
      var calleeName = ""
      var i = 0
      breakable {
        for (node <- nodes) {
          i = i + 1
          node match {
            case IdentifierNode(value) => calleeName = calleeName + value
            case KeywordNode(".")      => calleeName = calleeName + "."
            case _                     => break
          }
        }
      }

      val parameters = nodes
        .find(_.isInstanceOf[ExpressionListNode])
        .fold(ExpressionListNode(List()))(_.asInstanceOf[ExpressionListNode])
      SubroutineCallNode(calleeName, parameters, nodes).pure[List]
    }
  }
}

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
