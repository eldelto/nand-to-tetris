package net.eldelto.nand2tetris.jackcompiler

import cats.implicits._
import org.apache.commons.text.StringEscapeUtils
import scala.collection.mutable.ListBuffer

class CodeGenerator {
  private val symbolTable = SymbolTable()

  def generate(nodes: List[ASTNode]): List[String] = nodes.flatMap(generate)

  def generate(node: ASTNode): List[String] = {
    node match {
      case IdentifierNode(value)      => List()
      case IntegerConstantNode(value) => List()
      case StringConstantNode(value)  => resolveStringConstant(value)
      case KeywordNode(value) =>
        if (value.length == 1)
          List()
        else
          List()
      case ClassNode(name, children) =>
        symbolTable.className = name
        generate(children)
      case ClassVarDecNode(declarations, children) =>
        declarations.foreach(symbolTable.addClassDeclaration(_))
        List()
      case VarDecNode(declarations, children) =>
        declarations.foreach(symbolTable.addSubroutineDeclaration(_))
        List()
      case SubroutineDecNode(routineType, name, _, parameters, children) =>
        symbolTable.clearSubroutineTable()
        resolveSubroutineDecNode(
          symbolTable,
          routineType,
          name,
          parameters
        ) ++ generate(children)
      case ParameterListNode(children)  => generate(children)
      case SubroutineBodyNode(children) => generate(children)
      case StatementsNode(children)     => generate(children)
      case n: LetStatementNode   => resolveLetStatement(n)
      case IfStatementNode(children)    => generate(children)
      case WhileStatementNode(children) => generate(children)
      case n: SubroutineCallNode => resolveSubroutineCall(n)
      case n: DoStatementNode => resolveDoStatement(n)
      case ReturnStatementNode(children) =>
        generate(children) ++ resolveReturnStatement(children)
      case n: ExpressionNode => resolveExpression(n)
      case n: TermNode       => resolveTerm(n)
      case ExpressionListNode(children) =>
        generate(children)
      case _ => List()
    }
  }

  private def resolveExpression(expression: ExpressionNode): List[String] =
    expression.children.length match {
      case 1 => resolveTerm(expression.children(0).asInstanceOf[TermNode])
      case _ =>
        infixToPostfixNotation(expression.children).flatMap(_ match {
          case ops: KeywordNode => resolveOps(ops).pure[List]
          case term: TermNode   => resolveTerm(term)
          case node =>
            throw IllegalStateException(s"Not a valid expression node: $node")
        })
    }

  private def resolveTerm(term: TermNode): List[String] = term match {
    case LiteralTermNode(literal)     => resolveLiteral(literal)
    case PriorityTermNode(expression) => resolveExpression(expression)
    case IdentifierTermNode(identifier) => resolveIdentifier(identifier)
    case GenericTermNode(children) => generate(children) // TODO: Figure out what to do here.
  }

  private def resolveLiteral(literal: LiteralNode): List[String] = literal match {
    case IntegerConstantNode(value) => List(s"push constant $value")
    case StringConstantNode(value)  => resolveStringConstant(value)
    case KeywordNode(value)         => List(s"tbd '$value'") // TODO: Handle different keywords.
  }

  private def resolveIdentifier(identifier: String): List[String] = {
    val symbol = symbolTable.getSymbol(identifier)
    List(s"push local ${symbol.index}")
  }

  private def resolveOps(node: KeywordNode): String = node match {
    case KeywordNode("+") => "add"
    case KeywordNode("*") => "call Math.multiply 2"
    case KeywordNode("/") => "call Math.divide 2"
    case KeywordNode("<") => "lt"
    case KeywordNode(">") => "gt"
    case KeywordNode(k)   => s"tbd '$k'" // TODO: Handle other operations.
  }

  private def infixToPostfixNotation(nodes: List[ASTNode]): List[ASTNode] = {
    val result = ListBuffer(nodes.head)
    var i = 2
    while (i < nodes.length) {
      result.append(nodes(i))
      result.append(nodes(i - 1))
      i += 2
    }

    result.toList
  }

  private def resolveDoStatement(node: DoStatementNode): List[String] = {
    resolveSubroutineCall(node.callee) ++
    List("pop temp 0")
  }

  private def resolveReturnStatement(children: List[ASTNode]): List[String] = {
    val result = if (children.length == 1) List("push constant 0") else List()
    result ++ List("return")
  }

  private def resolveSubroutineDecNode(
      symbolTable: SymbolTable,
      routineType: SubroutineType,
      name: String,
      parameters: ParameterListNode
  ): List[String] = {
    // TODO: Fix parameter count.
    val parametercount = parameters.children.length

    routineType match {
      case SubroutineType.Function =>
        List(s"function ${symbolTable.className}.$name $parametercount")
      case value => List(s"$value TBD")
    }
  }

  private def resolveStringConstant(value: String): List[String] = {
    val charValues = value.map(_.intValue)
    List(s"push constant ${value.length}", "call String.new 1") ++
    charValues.toList.flatMap(v => List(s"push constant $v", "call String.appendChar 2"))
  }

  private def resolveSubroutineCall(node: SubroutineCallNode): List[String] = {
    val parameterCount = node.parameters.children.size
    generate(node.children) ++ 
    List(s"call ${node.calleeName} ${parameterCount}")
  }

  private def resolveLetStatement(node: LetStatementNode): List[String] = {
    val symbol = symbolTable.getSymbol(node.variableName)
    generate(node.children) ++
    List(s"pop local ${symbol.index}")
  }
}
