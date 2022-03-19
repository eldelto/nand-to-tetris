package net.eldelto.nand2tetris.vmtranslator

import scala.io._
import java.nio.file._
import collection.JavaConverters._

def getAsmPath(inputFile: Path): Path = {
  val outFilename = inputFile.getFileName.toString.replace(".vm", ".hack")
  inputFile.getParent.resolve(outFilename)
}

@main def main(filename: String): Unit = {
  val inputPath = Path.of(filename)
  val source = Source.fromFile(filename)

  try {
    val instructions = Stream.from(source.getLines)
    Translator.translate(Stream.from(instructions)) match {
      case Left(error) => println("Error: " + error)
      case Right(asm) =>
        val outputPath = getAsmPath(inputPath)
        Files.write(
          outputPath,
          asm.asJava
        )
    }
  } finally {
    source.close()
  }
}
