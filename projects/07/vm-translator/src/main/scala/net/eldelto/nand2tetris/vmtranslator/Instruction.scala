package net.eldelto.nand2tetris.vmtranslator

sealed trait Instruction {
  def toAssembly(): List[String]
}

private def increaseSP(): List[String] = List(
  "@SP",
  "M=M+1"
)

private def decreaseSP(): List[String] = List(
  "@SP",
  "M=M-1"
)

case class Comment() extends Instruction {
  override def toAssembly(): List[String] = List()
}

case class PushConstant(val x: Int) extends Instruction {
  override def toAssembly(): List[String] = List(
    s"// push $x",
    s"@$x",
    "D=A",
    "@SP",
    "A=M",
    "M=D"
  ) ++ increaseSP()
}

case class Pop() extends Instruction {
  override def toAssembly(): List[String] = List(
    "// pop",
    "@SP",
    "A=M",
    "D=M"
  ) ++ decreaseSP()
}

case class Add() extends Instruction {
  override def toAssembly(): List[String] = List("// add")
    ++ decreaseSP() ++
    List("A=M", "D=M", "@SP", "A=M-1", "D=M+D", "M=D")
}

/*
Command Return value Return value
add x + y integer
sub x - y integer
neg -y integer
eq x==0 boolean
gt x > y boolean
lt x < y boolean
and x and y boolean
or x or y boolean
not not x boolean
 */
