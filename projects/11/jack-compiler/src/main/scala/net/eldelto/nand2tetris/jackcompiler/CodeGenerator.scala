package net.eldelto.nand2tetris.jackcompiler

import cats.implicits._
import org.apache.commons.text.StringEscapeUtils
import scala.collection.mutable.ListBuffer

class CodeGenerator {
  private val symbolTable = SymbolTable()

  def generate(nodes: List[ASTNode]): List[String] = nodes.flatMap(generate)

  def generate(node: ASTNode): List[String] = {
    node match {
      case IdentifierNode(value) =>
        s"<identifier> ${StringEscapeUtils.escapeXml11(value)} </identifier>"
          .pure[List]
      case IntegerConstantNode(value) =>
        s"<integerConstant> $value </integerConstant>".pure[List]
      case StringConstantNode(value) =>
        s"<stringConstant> ${StringEscapeUtils.escapeXml11(value)} </stringConstant>"
          .pure[List]
      case KeywordNode(value) =>
        if (value.length == 1)
          s"<symbol> ${StringEscapeUtils.escapeXml11(value)} </symbol>"
            .pure[List]
        else
          s"<keyword> ${StringEscapeUtils.escapeXml11(value)} </keyword>"
            .pure[List]
      case ClassNode(_, children) => encloseChildren("class", children)
      case ClassVarDecNode(declarations, children) =>
        declarations.foreach(symbolTable.addClassDeclaration(_))
        List()
      case VarDecNode(declarations, children) =>
        declarations.foreach(symbolTable.addSubroutineDeclaration(_))
        List()
      case SubroutineDecNode(_, _, _, _, children) =>
        symbolTable.clearSubroutineTable()
        List()
      case ParameterListNode(children) =>
        encloseChildren("parameterList", children)
      case SubroutineBodyNode(children) =>
        encloseChildren("subroutineBody", children)
      case StatementsNode(children) => encloseChildren("statements", children)
      case LetStatementNode(children) =>
        encloseChildren("letStatement", children)
      case IfStatementNode(children) => encloseChildren("ifStatement", children)
      case WhileStatementNode(children) =>
        encloseChildren("whileStatement", children)
      case DoStatementNode(children) => encloseChildren("doStatement", children)
      case ReturnStatementNode(children) =>
        encloseChildren("returnStatement", children)
      case ExpressionNode(children)  => encloseChildren("expression", children)
      case n: TermNode => resolveTerm(n)
      case ExpressionListNode(children) =>
        encloseChildren("expressionList", children)
    }
  }
}

private def encloseChildren(
    tagName: String,
    children: List[ASTNode]
): List[String] =
  s"<$tagName>".pure[List] ++
    children.map(writeXml(_)).flatten ++
    s"</$tagName>".pure[List]

private def resolveExpression(expression: ExpressionNode): List[String] =  expression.children.length match {
    case 1 => resolveTerm(expression.children(0).asInstanceOf[TermNode])
    case _ => infixToPostfixNotation(expression.children).flatMap( _ match {
      case ops: KeywordNode => resolveOps(ops).pure[List]
      case term: TermNode => resolveTerm(term)
      case node => throw IllegalStateException(s"Not a valid expression node: $node")
    })
}

private def resolveTerm(term: TermNode): List[String] = term match {
  case LiteralTermNode(literal) => resolveLiteral(literal).pure[List]
  case PriorityTermNode(expression) => resolveExpression(expression)
  case t: GenericTermNode => List() // TODO: Figure out what to do here.
}

private def resolveLiteral(literal: LiteralNode): String = literal match {
  case IntegerConstantNode(value) => s"push $value"
  case StringConstantNode(value) => "" // TODO: Allocate new String.
  case KeywordNode(value) => "" // TODO: Handle different keywords.
}

private def resolveOps(node: KeywordNode): String = node match {
  case KeywordNode("+") => "add"
  case KeywordNode("*") => "multiply"
  case _ => "" // TODO: Handle other operations.
}

private def infixToPostfixNotation(nodes: List[ASTNode]): List[ASTNode] = {
  val result = ListBuffer(nodes.head)
  var i = 2
  while(i < nodes.length) {
    result.append(nodes(i))
    result.append(nodes(i-1))
    i += 2
  }

  result.toList
}