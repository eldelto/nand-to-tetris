package net.eldelto.nand2tetris.jackcompiler

import cats.implicits._
import org.apache.commons.text.StringEscapeUtils
import scala.collection.mutable.ListBuffer

class CodeGenerator {
  private val symbolTable = SymbolTable()
  private var whileIndex = 0

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
      case n: LiteralNode => resolveLiteral(n)
      case ClassNode(name, children) =>
        symbolTable.className = name
        generate(children)
      case ClassVarDecNode(declarations, children) =>
        declarations.foreach(symbolTable.addClassDeclaration(_))
        List()
      case VarDecNode(declarations, children) =>
        declarations.foreach(symbolTable.addSubroutineDeclaration(_))
        List()
      case n: SubroutineDecNode =>
        symbolTable.clearSubroutineTable()
        resolveSubroutineDecNode(n)
      case ParameterListNode(children)  => generate(children)
      case SubroutineBodyNode(children) => generate(children)
      case StatementsNode(children)     => generate(children)
      case n: LetStatementNode          => resolveLetStatement(n)
      case n: ArrayLetStatementNode     => resolveArrayLetStatement(n)
      case IfStatementNode(children)    => generate(children)
      case n: WhileStatementNode        => resolveWhileStatement(n)
      case n: SubroutineCallNode        => resolveSubroutineCall(n)
      case n: DoStatementNode           => resolveDoStatement(n)
      case ReturnStatementNode(children) =>
        generate(children) ++ resolveReturnStatement(children)
      case n: ExpressionNode => resolveExpression(n)
      case n: TermNode       => resolveTerm(n)
      case ExpressionListNode(children) =>
        generate(children)
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
    case LiteralTermNode(literal)       => resolveLiteral(literal)
    case PriorityTermNode(expression)   => resolveExpression(expression)
    case IdentifierTermNode(identifier) => resolveIdentifier(identifier)
    case n: ArrayIdentifierTermNode => resolveArrayIdentifier(n)
    case GenericTermNode(children) =>
      generate(children) // TODO: Figure out what to do here.
  }

  private def resolveLiteral(literal: LiteralNode): List[String] =
    literal match {
      case IntegerConstantNode(value) => List(s"push constant $value")
      case StringConstantNode(value)  => resolveStringConstant(value)
      case KeywordNode(value) =>
        List(s"tbd '$value'") // TODO: Handle different keywords.
    }

  private def resolveIdentifier(identifier: String): List[String] = {
    val symbol = symbolTable.getSymbol(identifier)
    List(s"push local ${symbol.index}")
  }

  private def resolveArrayIdentifier(node: ArrayIdentifierTermNode): List[String] = {
    val symbol = symbolTable.getSymbol(node.identifier)
    resolveExpression(node.index) ++
    List(s"push local ${symbol.index}", "add", "pop pointer 1", "push that 0")
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

  private def resolveSubroutineDecNode(node: SubroutineDecNode): List[String] = {
    // TODO: Fix parameter count.
    val parameterCount = node.parameters.children.length
    val localVariableCount = node.body.children
      .filter(_.isInstanceOf[VarDecNode])
      .map(_.asInstanceOf[VarDecNode].declarations.length)
      .sum

    val totalCount = parameterCount + localVariableCount

    node.routineType match {
      case SubroutineType.Function =>
        List(s"function ${symbolTable.className}.${node.name} $totalCount") ++
        generate(node.body.children)
      case value => List(s"$value TBD")
    }
  }

  private def resolveStringConstant(value: String): List[String] = {
    val charValues = value.map(_.intValue)
    List(s"push constant ${value.length}", "call String.new 1") ++
      charValues.toList.flatMap(v =>
        List(s"push constant $v", "call String.appendChar 2")
      )
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

  private def resolveArrayLetStatement(
      node: ArrayLetStatementNode
  ): List[String] = {
    val symbol = symbolTable.getSymbol(node.variableName)
    resolveExpression(node.indexExpression) ++
      List(s"push local ${symbol.index}") ++
      List("add") ++
      resolveExpression(node.expression) ++
      List("pop temp 0", "pop pointer 1", "push temp 0", "pop that 0")
  }

  private def resolveWhileStatement(node: WhileStatementNode): List[String] = {
    val startLabel = s"WHILE_EXP$whileIndex"
    val endLabel = s"WHILE_END$whileIndex"
    whileIndex += 1

    List(s"label $startLabel") ++
      resolveExpression(node.condition) ++
      List("not", s"if-goto $endLabel") ++
      generate(node.body) ++
      List(s"goto $startLabel", s"label $endLabel")
  }
}
