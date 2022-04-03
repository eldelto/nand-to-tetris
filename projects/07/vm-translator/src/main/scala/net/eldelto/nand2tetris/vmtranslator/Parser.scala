package net.eldelto.nand2tetris.vmtranslator

import scala.util.Either

case class ParsingError(val instruction: String)

class Parser() {
  private var index: Long = 0;

  private def dispatchPush(
      tokens: Array[String],
      filename: String
  ): Either[ParsingError, Instruction] = {
    val memorySegment = tokens(1)
    val offset = tokens(2).toInt

    memorySegment match {
      case "constant" => Right(PushConstant(offset))
      case "local" =>
        Right(PushMemorySegment(MemorySegment.LCL, offset, filename))
      case "argument" =>
        Right(PushMemorySegment(MemorySegment.ARG, offset, filename))
      case "this" =>
        Right(PushMemorySegment(MemorySegment.THIS, offset, filename))
      case "that" =>
        Right(PushMemorySegment(MemorySegment.THAT, offset, filename))
      case "static" =>
        Right(PushMemorySegment(MemorySegment.Static, offset, filename))
      case "temp" =>
        Right(PushMemorySegment(MemorySegment.Temp, offset, filename))
      case "pointer" =>
        Right(PushMemorySegment(MemorySegment.Pointer, offset, filename))
      case _ => Left(ParsingError(tokens.mkString(" ")))
    }
  }

  private def dispatchPop(
      tokens: Array[String],
      filename: String
  ): Either[ParsingError, Instruction] = {
    val memorySegment = tokens(1)
    val offset = tokens(2).toInt

    memorySegment match {
      case "local" =>
        Right(PopMemorySegment(MemorySegment.LCL, offset, filename))
      case "argument" =>
        Right(PopMemorySegment(MemorySegment.ARG, offset, filename))
      case "this" =>
        Right(PopMemorySegment(MemorySegment.THIS, offset, filename))
      case "that" =>
        Right(PopMemorySegment(MemorySegment.THAT, offset, filename))
      case "static" =>
        Right(PopMemorySegment(MemorySegment.Static, offset, filename))
      case "temp" =>
        Right(PopMemorySegment(MemorySegment.Temp, offset, filename))
      case "pointer" =>
        Right(PopMemorySegment(MemorySegment.Pointer, offset, filename))
      case _ => Left(ParsingError(tokens.mkString(" ")))
    }
  }

  private def dispatchBranching(
      tokens: Array[String]
  ): Either[ParsingError, Instruction] = {
    if (tokens.length < 2) return Left(ParsingError(tokens.mkString(" ")))

    val instruction = tokens(0)
    val label = tokens(1)

    instruction match {
      case "label"   => Right(Label(label))
      case "goto"    => Right(GoTo(label))
      case "if-goto" => Right(IfGoTo(label))
      case _         => Left(ParsingError(tokens.mkString(" ")))
    }
  }

  private def dispatchFunctionCall(
      tokens: Array[String]
  ): Either[ParsingError, Instruction] = {
    val instruction = tokens(0)

    if (instruction == "return") return Right(Return())
    if (tokens.length < 3) return Left(ParsingError(tokens.mkString(" ")))

    val functionName = tokens(1)
    val variableCount = tokens(2).toInt

    instruction match {
      case "function" => Right(Function(functionName, variableCount))
      case "call"     => Right(Call(functionName, variableCount, index))
      case _          => Left(ParsingError(tokens.mkString(" ")))
    }
  }

  private def dispatchOperation(
      tokens: Array[String]
  ): Either[ParsingError, Instruction] = {
    val operation = tokens(0)

    operation match {
      case "not" => Right(Not())
      case "neg" => Right(Negate())
      case "add" => Right(Add())
      case "sub" => Right(Substract())
      case "and" => Right(And())
      case "or"  => Right(Or())
      case "eq"  => Right(Equals(index))
      case "lt"  => Right(LessThan(index))
      case "gt"  => Right(GreaterThan(index))
      case _     => Left(ParsingError(tokens.mkString(" ")))
    }
  }

  def parse(
      rawInstruction: String,
      filename: String
  ): Either[ParsingError, Instruction] = {
    val trimmedInstruction = rawInstruction.trim
    if (trimmedInstruction.startsWith("//")) {
      return Right(Comment())
    }

    val tokens = trimmedInstruction.split("\\s").map(_.trim())
    val instruction = tokens(0) match {
      case "push" => dispatchPush(tokens, filename)
      case "pop"  => dispatchPop(tokens, filename)
      case _ =>
        dispatchOperation(tokens)
          .orElse(dispatchBranching(tokens))
          .orElse(dispatchFunctionCall(tokens))
    }

    index += 1
    instruction
  }
}
