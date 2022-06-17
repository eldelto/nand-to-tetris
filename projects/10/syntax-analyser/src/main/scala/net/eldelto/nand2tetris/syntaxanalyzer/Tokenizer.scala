package net.eldelto.nand2tetris.syntaxanalyzer

import cats.implicits._
import scala.collection.mutable.ListBuffer

trait Token {
  def value: String
}

enum Keyword(val value: String) extends Token {
  case Class extends Keyword("class")
  case Constructor extends Keyword("constructor")
  case Function extends Keyword("function")
  case Method extends Keyword("method")
  case Field extends Keyword("field")
  case Static extends Keyword("static")
  case Var extends Keyword("var")
  case Int extends Keyword("int")
  case Char extends Keyword("char")
  case Boolean extends Keyword("boolean")
  case Void extends Keyword("void")
  case True extends Keyword("true")
  case False extends Keyword("false")
  case Null extends Keyword("null")
  case This extends Keyword("this")
  case Let extends Keyword("let")
  case Do extends Keyword("do")
  case If extends Keyword("if")
  case Else extends Keyword("else")
  case While extends Keyword("while")
  case Return extends Keyword("return")
}
object Keyword {
  def parse(rawValue: String): Option[Keyword] =
    Keyword.values.find(_.value == rawValue)
}

enum Symbol(val value: String) extends Token {
  case RightCurly extends Symbol("}")
  case LeftCurly extends Symbol("{")
  case RightBracket extends Symbol("]")
  case LeftBracket extends Symbol("[")
  case RightParen extends Symbol(")")
  case LeftParen extends Symbol("(")
  case Period extends Symbol(".")
  case Comma extends Symbol(",")
  case SemiColon extends Symbol(";")
  case Plus extends Symbol("+")
  case Minus extends Symbol("-")
  case Star extends Symbol("*")
  case Slash extends Symbol("/")
  case Ampersand extends Symbol("&")
  case Pipe extends Symbol("|")
  case LessThan extends Symbol("<")
  case GreaterThan extends Symbol(">")
  case Equals extends Symbol("=")
  case Tilde extends Symbol("~")
}
object Symbol {
  def parse(rawValue: String): Option[Symbol] =
    Symbol.values.find(_.value == rawValue)
}

case class IntConstant(value: String) extends Token
object IntConstant {
  def parse(rawValue: String): Option[IntConstant] =
    rawValue.toIntOption.map(_ => IntConstant(rawValue))
}

case class StringConstant(value: String) extends Token
object StringConstant {
  def parse(rawValue: String): Option[StringConstant] =
    if (rawValue.startsWith("\"") && rawValue.endsWith("\""))
      StringConstant(rawValue).some
    else none
}

case class StringIdentifier(value: String) extends Token
object StringIdentifier {
  def parse(rawValue: String): Option[StringIdentifier] = StringIdentifier(
    rawValue
  ).some
}

def parseNonSymbol(rawValue: String): Token = Keyword
  .parse(rawValue)
  .orElse(IntConstant.parse(rawValue))
  .orElse(StringConstant.parse(rawValue))
  .orElse(StringIdentifier.parse(rawValue))
  .getOrElse(
    throw IllegalArgumentException(
      s"'$rawValue' is not a valid StringIdentifier"
    )
  )

def tokenize(inputLines: List[String]): List[Token] = {
  val charList = filterComments(inputLines)
    .map(_.toList)
    .flatten

  val tokens = parseTokens(charList)
  return combineStringConstants(tokens)
}

private def filterComments(inputLines: List[String]): List[String] = {
  var isBlockComment = false
  inputLines
    .map(_.split("//").head)
    .filter { line =>
      val containsStart = line.contains("/*")
      val containsEnd = line.contains("*/")
      if (containsStart) isBlockComment = true
      if (containsEnd) isBlockComment = false
      !(isBlockComment | containsStart | containsEnd)
    }
}

private def parseTokens(input: List[Char]): List[Token] = {
  var tokenBuffer: String = ""
  return input
    .map { char =>
      if (char == ' ') {
        val tokens = LazyList(parseNonSymbol(tokenBuffer))
        tokenBuffer = ""
        tokens
      } else {
        Symbol.parse(char.toString) match {
          case Some(symbolToken) =>
            val tokens = LazyList(parseNonSymbol(tokenBuffer), symbolToken)
            tokenBuffer = ""
            tokens
          case None =>
            tokenBuffer += char
            LazyList()
        }
      }
    }
    .flatten
    .filter(_.value.trim.nonEmpty)
}

private def combineStringConstants(tokens: List[Token]): List[Token] = {
  var isStringConstant = false
  var stringConstant = ""
  val result: ListBuffer[Token] = ListBuffer()
  for (token <- tokens) {
    token match {
      case StringIdentifier(value) if value.startsWith("\"") =>
        isStringConstant = true
        stringConstant = value
      case StringIdentifier(value) if value.endsWith("\"") =>
        isStringConstant = false
        stringConstant = stringConstant + " " + value
        result.addOne(StringConstant(stringConstant))
      case StringConstant(value) if value == "\"" =>
        if (isStringConstant) {
          stringConstant = stringConstant + " \""
          result.addOne(StringConstant(stringConstant))
        } else {
          stringConstant = "\" "
        }
        isStringConstant = !isStringConstant
      case _ if isStringConstant =>
        stringConstant = stringConstant + " " + token.value
      case _ => result.addOne(token)
    }
  }

  return result.toList
}
