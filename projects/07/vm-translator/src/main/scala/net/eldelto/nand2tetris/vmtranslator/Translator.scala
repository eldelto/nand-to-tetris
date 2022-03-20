package net.eldelto.nand2tetris.vmtranslator

import cats.syntax.traverse._

object Translator {
  def translate(
      vmInstructions: Stream[String]
  ): Either[ParsingError, Stream[String]] = {
    val parser = Parser()
    vmInstructions
      .filter(!_.isBlank)
      .map(parser.parse)
      .sequence
      .map(_.flatMap(_.toAssembly))
  }
}
