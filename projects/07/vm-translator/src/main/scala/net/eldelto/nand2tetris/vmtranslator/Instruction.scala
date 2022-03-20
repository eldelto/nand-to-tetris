package net.eldelto.nand2tetris.vmtranslator

enum BinaryOperation {
  case `M+D`
  case `M-D`
  case `D|M`
  case `D&M`
}

enum JumpComparison {
  case JGT
  case JEQ
  case JGE
  case JLT
  case JNE
  case JLE
  case JMP
}

sealed trait Instruction {
  def toAssembly: List[String]
}

inline private def increaseSP(): List[String] = List(
  "@SP",
  "M=M+1"
)

inline private def decreaseSP(): List[String] = List(
  "@SP",
  "M=M-1"
)

inline private def unaryOperation(operations: List[String]): List[String] =
  List("@SP", "A=M-1", "D=M") ++ operations ++ List("M=D")

inline private def binaryOperation(operation: BinaryOperation): List[String] =
  decreaseSP() ++
    List("A=M", "D=M", "@SP", "A=M-1", s"D=$operation", "M=D")

inline private def comparison(
    comparison: JumpComparison,
    index: Long
): List[String] =
  decreaseSP() ++
    List(
      "A=M",
      "D=M",
      "@SP",
      "A=M-1",
      "D=M-D",
      "M=-1",
      s"@CONTINUE_$index",
      s"D;$comparison",
      "@SP",
      "A=M-1",
      "M=0",
      s"(CONTINUE_$index)"
    )

case class Comment() extends Instruction {
  override val toAssembly: List[String] = List()
}

case class PushConstant(val x: Int) extends Instruction {
  override val toAssembly: List[String] = List(
    s"// push $x",
    s"@$x",
    "D=A",
    "@SP",
    "A=M",
    "M=D"
  ) ++ increaseSP()
}

case class Pop() extends Instruction {
  override val toAssembly: List[String] = List(
    "// pop",
    "@SP",
    "A=M",
    "D=M"
  ) ++ decreaseSP()
}

case class Not() extends Instruction {
  override val toAssembly: List[String] = List("// not")
    ++ unaryOperation(List("D=!D"))
}

case class Negate() extends Instruction {
  override val toAssembly: List[String] = List("// neg")
    ++ unaryOperation(List("D=D-1", "D=!D"))
}

case class Add() extends Instruction {
  override val toAssembly: List[String] = List("// add")
    ++ binaryOperation(BinaryOperation.`M+D`)
}

case class Substract() extends Instruction {
  override val toAssembly: List[String] = List("// sub")
    ++ binaryOperation(BinaryOperation.`M-D`)
}

case class And() extends Instruction {
  override val toAssembly: List[String] = List("// and")
    ++ binaryOperation(BinaryOperation.`D&M`)
}

case class Or() extends Instruction {
  override val toAssembly: List[String] = List("// or")
    ++ binaryOperation(BinaryOperation.`D|M`)
}

case class Equals(index: Long) extends Instruction {
  override val toAssembly: List[String] = List("// eq")
    ++ comparison(JumpComparison.JEQ, index)
}

case class LessThan(index: Long) extends Instruction {
  override val toAssembly: List[String] = List("// lt")
    ++ comparison(JumpComparison.JLT, index)
}

case class GreaterThan(index: Long) extends Instruction {
  override val toAssembly: List[String] = List("// gt")
    ++ comparison(JumpComparison.JGT, index)
}
