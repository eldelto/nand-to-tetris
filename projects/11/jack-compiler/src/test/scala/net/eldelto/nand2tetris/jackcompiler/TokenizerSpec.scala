package net.eldelto.nand2tetris.jackcompiler

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.util.stream.Collectors
import scala.jdk.StreamConverters._

class TokenizerSpec extends AnyFlatSpec with Matchers {
  "The tokenizer" should "properly tokenize the given input" in {
    val input = """/* Comment */
    /* Multi
    line
    comment */
    // Comment
    class MyClass {
      field int x = 0; // Comment
      field String y = "string constant "; // Comment
    }"""

    val expected = List(
      Keyword.Class,
      StringIdentifier("MyClass"),
      Symbol.LeftCurly,
      Keyword.Field,
      Keyword.Int,
      StringIdentifier("x"),
      Symbol.Equals,
      IntConstant("0"),
      Symbol.SemiColon,
      Keyword.Field,
      StringIdentifier("String"),
      StringIdentifier("y"),
      Symbol.Equals,
      StringConstant("\"string constant \""),
      Symbol.SemiColon,
      Symbol.RightCurly,
    )

    val result = tokenize(input.lines.toScala(List))
    result shouldBe expected
  }
}
