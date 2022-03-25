package net.eldelto.nand2tetris.vmtranslator

enum MemorySegment {
  case LCL
  case ARG
  case THIS
  case THAT
  case Static
  case Temp
}

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

private def storeDInR13(): List[String] = List(
  "@R13",
  "M=D"
)

private def segmentAddressToR13(
    segment: MemorySegment,
    offset: Int
): List[String] = {
  val addressInD = segment match {
    case MemorySegment.Static =>
      List(
        "@STATIC_" + offset,
        "D=A"
      )
    case MemorySegment.Temp =>
      List(
        "@R" + (offset + 5),
        "D=A"
      )
    case _ =>
      List(
        "@" + segment,
        "D=M",
        "@" + offset,
        "D=A+D"
      )

  }
  val storeInR13 = List(
  )
}

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

case class PopMemorySegment(val segment: MemorySegment, offset: Int)
    extends Instruction {
  override val toAssembly: List[String] = List(s"// pop $segment")
    ++ segmentAddressToR13(segment, offset)
    ++ decreaseSP()
    ++ List(
      "@SP",
      "A=M",
      "D=M",
      "@R13",
      "A=M",
      "M=D"
    )
}

case class PushMemorySegment(val segment: MemorySegment, offset: Int)
    extends Instruction {
  override val toAssembly: List[String] = List(s"// pop $segment")
    ++ segmentAddressToR13(segment, offset)
    ++ List(
      "@R13",
      "A=M",
      "D=M",
      "@SP",
      "A=M",
      "M=D"
    )
    ++ increaseSP()
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
