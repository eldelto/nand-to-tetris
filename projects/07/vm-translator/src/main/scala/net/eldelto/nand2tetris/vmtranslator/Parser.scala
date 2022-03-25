package net.eldelto.nand2tetris.vmtranslator

import scala.util.Either

case class ParsingError(val instruction: String)

class Parser() {
  private var index: Long = 0;

  private def dispatchPush(
      tokens: Array[String]
  ): Either[ParsingError, Instruction] = {
    val memorySegment = tokens(1)
    val offset = tokens(2).toInt

    memorySegment match {
      case "constant" => Right(PushConstant(offset))
      case "local"    => Right(PushMemorySegment(MemorySegment.LCL, offset))
      case "argument" => Right(PushMemorySegment(MemorySegment.ARG, offset))
      case "this"     => Right(PushMemorySegment(MemorySegment.THIS, offset))
      case "that"     => Right(PushMemorySegment(MemorySegment.THAT, offset))
      case "static"   => Right(PushMemorySegment(MemorySegment.Static, offset))
      case "temp"     => Right(PushMemorySegment(MemorySegment.Temp, offset))
      case "pointer"  => Right(PushMemorySegment(MemorySegment.Pointer, offset))
      case _          => Left(ParsingError(tokens.mkString(" ")))
    }
  }

  private def dispatchPop(
      tokens: Array[String]
  ): Either[ParsingError, Instruction] = {
    val memorySegment = tokens(1)
    val offset = tokens(2).toInt

    memorySegment match {
      case "local"    => Right(PopMemorySegment(MemorySegment.LCL, offset))
      case "argument" => Right(PopMemorySegment(MemorySegment.ARG, offset))
      case "this"     => Right(PopMemorySegment(MemorySegment.THIS, offset))
      case "that"     => Right(PopMemorySegment(MemorySegment.THAT, offset))
      case "static"   => Right(PopMemorySegment(MemorySegment.Static, offset))
      case "temp"     => Right(PopMemorySegment(MemorySegment.Temp, offset))
      case "pointer"  => Right(PopMemorySegment(MemorySegment.Pointer, offset))
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

  def parse(rawInstruction: String): Either[ParsingError, Instruction] = {
    val trimmedInstruction = rawInstruction.trim
    if (trimmedInstruction.startsWith("//")) {
      return Right(Comment())
    }

    val tokens = trimmedInstruction.split(" ")
    val instruction = tokens(0) match {
      case "push" => dispatchPush(tokens)
      case "pop"  => dispatchPop(tokens)
      case _      => dispatchOperation(tokens)
    }

    index += 1
    instruction
  }
}
