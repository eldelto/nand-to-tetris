package net.eldelto.nand2tetris.vmtranslator

import scala.util.Either

case class ParsingError(val instruction: String)

object Parser {
  private def dispatchPush(
      tokens: Array[String]
  ): Either[ParsingError, Instruction] = {
    val memorySegment = tokens(1)

    memorySegment match {
      case "constant" => Right(PushConstant(tokens(2).toInt))
      case _          => Left(ParsingError(tokens.mkString(" ")))
    }
  }

  private def dispatchPop(
      tokens: Array[String]
  ): Either[ParsingError, Instruction] = {
    return Left(ParsingError(tokens.mkString(" ")))
  }

  private def dispatchOperation(
      tokens: Array[String]
  ): Either[ParsingError, Instruction] = {
    val operation = tokens(0)

    operation match {
      case "add" => Right(Add())
      case _     => Left(ParsingError(tokens.mkString(" ")))
    }
  }

  def parse(rawInstruction: String): Either[ParsingError, Instruction] = {
    val tokens = rawInstruction.split(" ")
    tokens(0) match {
      case "push" => dispatchPush(tokens)
      case "pop"  => dispatchPop(tokens)
      case _      => dispatchOperation(tokens)
    }
  }
}
