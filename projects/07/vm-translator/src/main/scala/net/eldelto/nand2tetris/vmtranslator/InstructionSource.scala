package net.eldelto.nand2tetris.vmtranslator

import scala.io._
import java.nio.file._

trait InstructionSource {
  def instructionStream: Stream[String]
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

  override def instructionStream: Stream[String] =
    Stream.from(source.getLines)

  override def close(): Unit = source.close
}

class DirectoryInstructionSource(val path: Path) extends InstructionSource {
  assert(path.toFile.isDirectory)

  private val sources: Stream[Source] =
    Stream.from(
      path.toFile.listFiles
        .filter(file => file.isFile && file.getName.endsWith(".vm"))
        .map(Source.fromFile(_))
    )

  override def instructionStream: Stream[String] =
    sources.map((s: Source) => Stream.from(s.getLines)).flatten

  override def close(): Unit = sources.foreach(_.close)
}
