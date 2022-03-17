package net.eldelto.nand2tetris.vmtranslator

import cats.syntax.traverse._

@main def main(): Unit = {
  val instructions = List("push constant 0", "push constant 2", "add")
  translate(Stream.from(instructions)) match {
    case Right(asm)  => asm.foreach(println(_))
    case Left(error) => println("Error: " + error)
  }
}

def translate(
    vmInstructions: Stream[String]
): Either[ParsingError, Stream[String]] = vmInstructions
  .map(Parser.parse)
  .sequence
  .map(_.flatMap(_.toAssembly()))
