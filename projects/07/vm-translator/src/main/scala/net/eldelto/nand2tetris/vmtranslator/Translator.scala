package net.eldelto.nand2tetris.vmtranslator

import cats.syntax.traverse._

object Translator {
  def translate(
      vmInstructions: Stream[String]
  ): Either[ParsingError, Stream[String]] = vmInstructions
    .filter(!_.isBlank)
    .map(Parser.parse)
    .sequence
    .map(_.flatMap(_.toAssembly()))
}
