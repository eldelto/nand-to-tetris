package net.eldelto.nand2tetris.jackcompiler

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class CodeGeneratorSpec
    extends AnyFlatSpec
    with Matchers
    with TableDrivenPropertyChecks {
  "The code generator" should "generate the proper VM code" in {
    val testData = Table[List[ASTNode], List[String]](
      ("AST nodes", "VM code"),
      (List(LiteralTermNode(IntegerConstantNode(1))), List("push constant 1")),
      (
        List(
          PriorityTermNode(
            ExpressionNode(
              List(
                LiteralTermNode(IntegerConstantNode(1)),
                KeywordNode("+"),
                LiteralTermNode(IntegerConstantNode(2))
              )
            )
          )
        ),
        List("push constant 1", "push constant 2", "add")
      )
    )

    forAll(testData) { (nodes, expected) =>
      val result = CodeGenerator().generate(nodes)
      result shouldBe expected
    }
  }
}
