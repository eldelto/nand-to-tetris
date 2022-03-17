package net.eldelto.nand2tetris.vmtranslator

sealed trait Instruction {
  def toAssembly(): List[String]
}

case class PushConstant(val x: Int) extends Instruction {
  def toAssembly(): List[String] = List(
    s"// push $x",
    s"@$x",
    "D=A",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1"
  )
}

case class Pop() extends Instruction {
  def toAssembly(): List[String] = List(
    "// pop",
    "@SP",
    "A=M",
    "D=M",
    "@SP",
    "M=M-1"
  )
}

case class Add() extends Instruction {
  def toAssembly(): List[String] = List(
    "// add",
    "@SP",
    "A=M",
    "D=M",
    "@SP",
    "M=M-1",
    "A=M",
    "M=M+D"
  )
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
