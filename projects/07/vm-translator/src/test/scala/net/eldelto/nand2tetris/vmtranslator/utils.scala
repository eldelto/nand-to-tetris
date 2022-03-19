package net.eldelto.nand2tetris.vmtranslator

import java.nio.file._
import scala.io.Source

trait TestResources {
  def resourcePath(): Path = Paths.get(getClass.getResource("/").toURI)

  def readResource(name: String): String = {
    val path = resourcePath().resolve(name)
    Files.readString(path)
  }
}
