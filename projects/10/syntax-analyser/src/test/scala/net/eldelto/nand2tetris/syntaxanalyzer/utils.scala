package net.eldelto.nand2tetris.syntaxanalyzer

import java.nio.file._
import scala.io.Source

trait TestResources {
  def resourcePath: Path = Paths.get(getClass.getResource("/").toURI)

  def resourcePath(name: String): Path = resourcePath.resolve(name)
}
