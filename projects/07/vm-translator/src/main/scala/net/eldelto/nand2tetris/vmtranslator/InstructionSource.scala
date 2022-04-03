package net.eldelto.nand2tetris.vmtranslator

import scala.io._
import java.nio.file._

trait InstructionSource {
  def instructionStream: Stream[RawInstruction]
  def close(): Unit
}

object InstructionSource {
  def from(path: Path): InstructionSource =
    if (path.toFile.isFile) return FileInstructionSource(path)
    else return DirectoryInstructionSource(path)
}

class FileInstructionSource(val path: Path) extends InstructionSource {
  assert(path.toFile.isFile)

  private val source: Source = Source.fromFile(path.toFile)
  private val filename: String = path.getFileName.toString

  override def instructionStream: Stream[RawInstruction] =
    Stream.from(source.getLines).map(RawInstruction(_, filename))

  override def close(): Unit = source.close
}

class DirectoryInstructionSource(val path: Path) extends InstructionSource {
  assert(path.toFile.isDirectory)

  private val sourceTuples: Stream[(Source, String)] =
    Stream.from(
      path.toFile.listFiles
        .filter(file => file.isFile && file.getName.endsWith(".vm"))
        .map(file => (Source.fromFile(file), file.getName))
    )

  override def instructionStream: Stream[RawInstruction] =
    sourceTuples.map { s =>
      Stream
        .from(s._1.getLines)
        .map(line => RawInstruction(line, s._2))
    }.flatten

  override def close(): Unit = sourceTuples.foreach(_._1.close)
}
