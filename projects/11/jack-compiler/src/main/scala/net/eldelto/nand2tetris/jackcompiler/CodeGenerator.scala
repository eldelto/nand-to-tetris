package net.eldelto.nand2tetris.jackcompiler

import cats.implicits._
import org.apache.commons.text.StringEscapeUtils

class CodeGenerator {
  private val symbolTable = SymbolTable()

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
      case ExpressionNode(children) => encloseChildren("expression", children)
      case TermNode(children)       => encloseChildren("term", children)
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
