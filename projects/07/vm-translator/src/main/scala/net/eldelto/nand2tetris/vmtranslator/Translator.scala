package net.eldelto.nand2tetris.vmtranslator

import cats.syntax.traverse._
import cats.syntax.either._
import java.nio.file._
import collection.JavaConverters._

case class RawInstruction(instruction: String, filename: String)

object Translator {
  def translate(
      path: Path,
      withBootstrapCode: Boolean = false
  ): Either[ParsingError, Unit] = {
    val source = InstructionSource.from(path)
    val outputPath = getAsmPath(path)

    try {
      for {
        bootstrap <- Right(bootstrapCode)
        translatedAsm <- translateInstructions(source.instructionStream)
      } yield {
        val asm =
          if (withBootstrapCode) bootstrap.concat(translatedAsm)
          else translatedAsm
        Files.write(outputPath, asm.asJava)
      }
    } finally {
      source.close()
    }
  }

  private def translateInstructions(
      vmInstructions: Stream[RawInstruction]
  ): Either[ParsingError, Stream[String]] = {
    val parser = Parser()
    vmInstructions
      .filter(!_.instruction.isBlank)
      .map(x => parser.parse(x.instruction, x.filename))
      .sequence
      .map(_.flatMap(_.toAssembly))
  }

  private def getAsmPath(path: Path): Path = {
    if (path.toFile.isFile) {
      val outFilename = path.getFileName.toString.replace(".vm", ".asm")
      path.getParent.resolve(outFilename)
    } else {
      path.resolve(path.getFileName.toString() + ".asm")
    }
  }

  private val bootstrapCode: Stream[String] = Stream.from(
    List(
      "@256",
      "D=A",
      "@SP",
      "M=D"
    )
      ++ Call("Sys.init", 0, 0).toAssembly
  )
}
