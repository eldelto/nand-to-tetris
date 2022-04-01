package net.eldelto.nand2tetris.vmtranslator

enum MemorySegment {
  case LCL
  case ARG
  case THIS
  case THAT
  case Static
  case Temp
  case Pointer
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

inline private def storePointer() = List(
  "A=M",
  "M=D"
)

inline private def loadPointer() = List(
  "A=M",
  "D=M"
)

inline private def storeDInR13() = List(
  "@R13",
  "M=D"
)

private def segmentAddressToR13(
    segment: MemorySegment,
    offset: Int
): List[String] = {
  val addressInD = segment match {
    case MemorySegment.Static =>
      List("@STATIC_" + offset, "D=A")
    case MemorySegment.Temp =>
      List("@R" + (offset + 5), "D=A")
    case MemorySegment.Pointer =>
      List(if (offset == 0) "@THIS" else "@THAT", "D=A")
    case _ =>
      List("@" + segment, "D=M", "@" + offset, "D=A+D")
  }

  List(
    addressInD,
    storeDInR13()
  ).flatten
}

inline private def unaryOperation(operations: List[String]) = List(
  List("@SP", "A=M-1", "D=M"),
  operations,
  List("M=D")
).flatten

inline private def binaryOperation(operation: BinaryOperation) = List(
  decreaseSP(),
  loadPointer(),
  List("@SP", "A=M-1", s"D=$operation", "M=D")
).flatten

inline private def comparison(
    comparison: JumpComparison,
    index: Long
) = List(
  decreaseSP(),
  loadPointer(),
  List(
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
).flatten

case class Comment() extends Instruction {
  override val toAssembly: List[String] = List()
}

case class PushConstant(val x: Int) extends Instruction {
  override val toAssembly: List[String] = List(
    List("// push " + x, "@" + x, "D=A", "@SP"),
    storePointer(),
    increaseSP()
  ).flatten
}

case class PopMemorySegment(val segment: MemorySegment, offset: Int)
    extends Instruction {
  override val toAssembly: List[String] = List(
    List("// pop " + segment),
    segmentAddressToR13(segment, offset),
    decreaseSP(),
    List("@SP"),
    loadPointer(),
    List("@R13"),
    storePointer()
  ).flatten
}

case class PushMemorySegment(val segment: MemorySegment, offset: Int)
    extends Instruction {
  override val toAssembly: List[String] = List(
    List("// pop " + segment),
    segmentAddressToR13(segment, offset),
    List("@R13"),
    loadPointer(),
    List("@SP"),
    storePointer(),
    increaseSP()
  ).flatten
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

case class Label(label: String) extends Instruction {
  override val toAssembly: List[String] = List(
    "// label " + label,
    s"($label)"
  )
}

case class GoTo(label: String) extends Instruction {
  override val toAssembly: List[String] = List(
    "// goto " + label,
    "@" + label,
    ";JMP"
  )
}

case class IfGoTo(label: String) extends Instruction {
  override val toAssembly: List[String] = List(
    List("// if-goto " + label, "@SP"),
    decreaseSP(),
    loadPointer(),
    List("@" + label, "D;JNE")
  ).flatten
}
