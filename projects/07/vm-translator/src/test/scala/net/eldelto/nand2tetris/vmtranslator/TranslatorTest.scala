package net.eldelto.nand2tetris.vmtranslator

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues
import java.nio.file.Files
import collection.JavaConverters._

class TranslatorSpec
    extends AnyFlatSpec
    with Matchers
    with EitherValues
    with TestResources {
  "The translator" should "properly translate the given files" in {
    val files = List("SimpleAdd")

    files.foreach { filename =>
      val vmInstructions = readResource(filename + ".vm").split("\n")
      val result = Translator.translate(Stream.from(vmInstructions))

      result.right.value shouldBe a[Stream[String]]

      Files.write(
        resourcePath().resolve(filename + ".asm"),
        result.getOrElse(Stream()).asJava
      )
    }
  }
}
