package net.eldelto.nand2tetris.syntaxanalyzer

import cats.implicits._

def writeXml(node: ASTNode): List[String] = {
  node match {
    case IdentifierNode(value) =>
      s"<identifier> $value </identifier>".pure[List]
    case KeywordNode(value)        => s"<keyword> $value </keyword>".pure[List]
    case ClassNode(children)       => encloseChildren("class", children)
    case ClassVarDecNode(children) => encloseChildren("classVarDec", children)
    case VarDecNode(children)      => encloseChildren("varDec", children)
    case SubroutineDecNode(children) =>
      encloseChildren("subroutineDec", children)
    case ParameterListNode(children) =>
      encloseChildren("parameterList", children)
    case SubroutineBodyNode(children) =>
      encloseChildren("subroutineBody", children)
    case StatementsNode(children)   => encloseChildren("statements", children)
    case LetStatementNode(children) => encloseChildren("letStatment", children)
    case IfStatementNode(children)  => encloseChildren("ifStatement", children)
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

private def encloseChildren(
    tagName: String,
    children: List[ASTNode]
): List[String] =
  s"<$tagName>".pure[List] ++
    children.map(writeXml(_)).flatten ++
    s"</$tagName>".pure[List]
