package net.eldelto.nand2tetris.syntaxanalyzer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import java.nio.file.Files
import scala.jdk.StreamConverters._

class XmlWriterSpec
    extends AnyFlatSpec
    with Matchers
    with TableDrivenPropertyChecks
    with TestResources {
  "The XML writer" should "emit the proper XML for a given code sample" in {
    val testData = Table(
      ("jackFile", "expectedXml"),
      ("Minimal.jack", "Minimal.xml")
      // ("ExpressionLessSquare/Main.jack", "ExpressionLessSquare/Main.xml")
    )

    forAll(testData) { (jackFile, expectedXml) =>
      val jackFileSource = Files.readString(resourcePath(jackFile))
      val expectedXmlSource = Files.readString(resourcePath(expectedXml))

      val tokens = tokenize(jackFileSource.lines.toScala(List))
      val ast = Class.execute(new ParserImpl(tokens))
      val result = ast
        .map(_.map(writeXml(_)).flatten)
        .getOrElse(List())
        .reduce((a, b) => a + "\n" + b)

        println(result)
      result shouldBe expectedXmlSource
    }
  }
}
