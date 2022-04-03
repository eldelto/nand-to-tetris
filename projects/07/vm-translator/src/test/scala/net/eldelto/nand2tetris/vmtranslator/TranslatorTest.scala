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
    val files =
      List(
        "SimpleAdd",
        "StackTest",
        "BasicTest",
        "StaticTest",
        "PointerTest",
        "BasicLoop",
        "FibonacciSeries",
        "SimpleFunction"
      )

    files.foreach { filename =>
      val path = resourcePath(filename + ".vm")
      val result = Translator.translate(path)

      result.right.value shouldBe a[Unit]
    }
  }

  "The translator" should "properly translate the given directories" in {
    val directories =
      List(
        "SimpleCall",
        "NestedCall",
        "FibonacciElement",
        "StaticsTest"
      )

    directories.foreach { directoryName =>
      val path = resourcePath(directoryName)
      val result = Translator.translate(path, true)

      result.right.value shouldBe a[Unit]
    }
  }
}
