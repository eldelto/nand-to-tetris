package net.eldelto.nand2tetris.vmtranslator

val SP = "SP"
val SEGMENT_ADDRESS = "R13"
val FRAME = "R14"
val RETURN = "R15"

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
  "@" + SP,
  "M=M+1"
)

inline private def decreaseSP(): List[String] = List(
  "@" + SP,
  "M=M-1"
)

inline private def storePointer(pointerAddress: String) = List(
  "@" + pointerAddress,
  "A=M",
  "M=D"
)

inline private def loadPointer(pointerAddress: String) = List(
  "@" + pointerAddress,
  "A=M",
  "D=M"
)

inline private def loadPointerWithOffset(pointerAddress: String, offset: Int) =
  List(
    "@" + pointerAddress,
    "D=M",
    "@" + offset,
    "A=D-A",
    "D=M"
  )

inline private def storeDInR13() = List(
  "@R13",
  "M=D"
)

inline private def popStack() = List(
  decreaseSP(),
  loadPointer(SP)
).flatten

inline private def pushStack() = List(
  storePointer(SP),
  increaseSP()
).flatten

inline private def pushSegmentPointer(segment: MemorySegment) = List(
  List("@" + segment, "D=M"),
  pushStack()
).flatten

inline private def restoreSegmentPointer(
    segment: MemorySegment,
    frameOffset: Int
) = List(
  loadPointerWithOffset(FRAME, frameOffset),
  List(
    "@" + segment,
    "M=D"
  )
).flatten

private def storeSegmentPointer(
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
    storeDInR13() // TODO: Inline
  ).flatten
}

inline private def unaryOperation(operations: List[String]) = List(
  List("@" + SP, "A=M-1", "D=M"),
  operations,
  List("M=D")
).flatten

inline private def binaryOperation(operation: BinaryOperation) = List(
  popStack(),
  List("@" + SP, "A=M-1", s"D=$operation", "M=D")
).flatten

inline private def comparison(
    comparison: JumpComparison,
    index: Long
) = List(
  popStack(),
  List(
    "@" + SP,
    "A=M-1",
    "D=M-D",
    "M=-1",
    s"@CONTINUE_$index",
    s"D;$comparison",
    "@" + SP,
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
    List("// push " + x, "@" + x, "D=A"),
    pushStack()
  ).flatten
}

case class PopMemorySegment(val segment: MemorySegment, offset: Int)
    extends Instruction {
  override val toAssembly: List[String] = List(
    List("// pop " + segment),
    storeSegmentPointer(segment, offset),
    popStack(),
    storePointer(SEGMENT_ADDRESS)
  ).flatten
}

case class PushMemorySegment(val segment: MemorySegment, offset: Int)
    extends Instruction {
  override val toAssembly: List[String] = List(
    List("// pop " + segment),
    storeSegmentPointer(segment, offset),
    loadPointer(SEGMENT_ADDRESS),
    pushStack()
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
    "0;JMP"
  )
}

case class IfGoTo(label: String) extends Instruction {
  override val toAssembly: List[String] = List(
    List("// if-goto " + label, "@" + SP),
    popStack(),
    List("@" + label, "D;JNE")
  ).flatten
}

case class Function(functionName: String, localVariableCount: Int)
    extends Instruction {
  override val toAssembly: List[String] = List(
    s"// function $functionName $localVariableCount",
    s"($functionName)"
  )
}

case class Call(functionName: String, argumentCount: Int, index: Long)
    extends Instruction {
  override val toAssembly: List[String] = List(
    List(
      s"// call $functionName $argumentCount",
      "(" + returnLabel(functionName, index) + ")",
      "@" + returnLabel(functionName, index),
      "D=M"
    ),
    pushStack(),
    pushSegmentPointer(MemorySegment.LCL),
    pushSegmentPointer(MemorySegment.ARG),
    pushSegmentPointer(MemorySegment.THIS),
    pushSegmentPointer(MemorySegment.THAT),
    List(
      "@" + SP,
      "D=M",
      "@" + (argumentCount + 5),
      "D=D-A",
      "@" + MemorySegment.ARG,
      "M=D",
      "@" + SP,
      "D=M",
      "@" + MemorySegment.LCL, // TODO: storeValue(MemorySegment.LCL)?
      "M=D",
      "@functionName",
      "0;JMP"
    )
  ).flatten

  private def returnLabel(functionName: String, index: Long) =
    functionName + "_return_" + index
}

case class Return() extends Instruction {
  override val toAssembly: List[String] = List(
    List(
      "// return",
      "@" + MemorySegment.LCL,
      "D=M",
      "@" + FRAME,
      "M=D"
    ),
    loadPointerWithOffset(FRAME, 5),
    List(
      "@" + RETURN,
      "M=D"
    ),
    popStack(),
    storePointer(MemorySegment.ARG.toString),
    List(
      "@" + MemorySegment.ARG,
      "D=M+1",
      "@" + SP,
      "M=D"
    ),
    restoreSegmentPointer(MemorySegment.THAT, 1),
    restoreSegmentPointer(MemorySegment.THIS, 2),
    restoreSegmentPointer(MemorySegment.ARG, 3),
    restoreSegmentPointer(MemorySegment.LCL, 4),
    List(
      "@" + RETURN,
      "A=M",
      "0;JMP"
    )
  ).flatten
}
