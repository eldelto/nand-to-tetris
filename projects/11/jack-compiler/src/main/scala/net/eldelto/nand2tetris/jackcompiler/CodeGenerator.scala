package net.eldelto.nand2tetris.jackcompiler

import cats.implicits._
import org.apache.commons.text.StringEscapeUtils
import scala.collection.mutable.ListBuffer

class CodeGenerator {
  private val symbolTable = SymbolTable()

  def generate(nodes: List[ASTNode]): List[String] = nodes.flatMap(generate)

  def generate(node: ASTNode): List[String] = {
    node match {
      case IdentifierNode(value) => List()
      case IntegerConstantNode(value) => List()
      case StringConstantNode(value) => List()
      case KeywordNode(value) =>
        if (value.length == 1)
          List()
        else
          List()
      case ClassNode(_, children) => generate(children)
      case ClassVarDecNode(declarations, children) =>
        declarations.foreach(symbolTable.addClassDeclaration(_))
        List()
      case VarDecNode(declarations, children) =>
        declarations.foreach(symbolTable.addSubroutineDeclaration(_))
        List()
      case SubroutineDecNode(_, _, _, _, children) =>
        symbolTable.clearSubroutineTable()
        generate(children)
      case ParameterListNode(children) => generate(children)
      case SubroutineBodyNode(children) => generate(children)
      case StatementsNode(children) => generate(children)
      case LetStatementNode(children) => generate(children)
      case IfStatementNode(children) => generate(children)
      case WhileStatementNode(children) => generate(children)
      case n @ DoStatementNode(_, _, children) => generate(children) ++ resolveDoStatement(n)
      case ReturnStatementNode(children) => generate(children)
      case n: ExpressionNode  => resolveExpression(n)
      case n: TermNode => resolveTerm(n)
      case ExpressionListNode(children) =>
        generate(children)
      case _ => List()
    }
  }
}

private def encloseChildren(tagName: String,children: List[ASTNode]): List[String] = List()
  

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
  case IntegerConstantNode(value) => s"push constant $value"
  case StringConstantNode(value) => "" // TODO: Allocate new String.
  case KeywordNode(value) => "" // TODO: Handle different keywords.
}

private def resolveOps(node: KeywordNode): String = node match {
  case KeywordNode("+") => "add"
  case KeywordNode("*") => "call Math.multiply 2"
  case _ => "<tbd>" // TODO: Handle other operations.
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

private def resolveDoStatement(node: DoStatementNode): List[String] = {
  val parameterCount = node.parameters.children.size
  List(s"call ${node.calleeName} ${parameterCount}", "pop")
}