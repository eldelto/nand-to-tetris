package net.eldelto.nand2tetris.jackcompiler

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import java.nio.file.Files
import scala.jdk.StreamConverters._
import java.nio.file.Path

class CompilerSpec
    extends AnyFlatSpec
    with Matchers
    with TableDrivenPropertyChecks
    with TestResources {
  "The compiler" should "emit the proper VM code for the given code sample" in {
    val testData = Table(
      ("Jack file", "Expected VM code"),
      ("Seven/Main.jack", "Seven/Main.vm"),
      ("Average/Main.jack", "Average/Main.vm"),
      // ("ComplexArrays/Main.jack", "ComplexArrays/Main.vm"),
      ("ConvertToBin/Main.jack", "ConvertToBin/Main.vm"),
      ("Square/Square.jack", "Square/Square.vm"),
      ("Square/SquareGame.jack", "Square/SquareGame.vm"),
    )

    forAll(testData) { (jackFile, expectedVmCode) =>
      val jackFileSource = Files.readString(resourcePath(jackFile))
      val expectedVmCodeSource = Files.readString(resourcePath(expectedVmCode))

      val tokens = tokenize(jackFileSource.lines.toScala(List))
      val ast = Class.execute(new ParserImpl(tokens))
      Files.writeString(Path.of("out.ast"), ast.toString())
      val result = ast
        .map(CodeGenerator().generate)
        .getOrElse(List())
        .reduce((a, b) => a + "\n" + b)

      Files.writeString(Path.of("out.vm"), result)
      result shouldBe expectedVmCodeSource
    }
  }
}
