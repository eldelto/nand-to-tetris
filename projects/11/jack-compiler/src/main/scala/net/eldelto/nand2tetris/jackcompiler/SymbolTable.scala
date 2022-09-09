package net.eldelto.nand2tetris.jackcompiler

import scala.collection.mutable.HashMap

case class SymbolEntry(declaration: SingleVariableDec, index: Int)

class SymbolTable {
  var className = ""
  private val classTable = HashMap[String, SymbolEntry]()
  private val classStaticTable = HashMap[String, SymbolEntry]()
  private val argumentTable = HashMap[String, SymbolEntry]()
  private val subroutineTable = HashMap[String, SymbolEntry]()
  private var argumentOffset = 0

  def addClassDeclaration(declaration: SingleVariableDec): Unit =
    classTable.put(declaration.name, SymbolEntry(declaration, classTable.size))

  def addClassStaticDeclaration(declaration: SingleVariableDec): Unit =
    classStaticTable.put(declaration.name, SymbolEntry(declaration.copy(variableType = VariableType.Static), classStaticTable.size))

  def addArgumentDeclaration(declaration: SingleVariableDec): Unit = argumentTable
    .put(declaration.name, SymbolEntry(declaration.copy(variableType = VariableType.Argument), argumentTable.size + argumentOffset))

  def addSubroutineDeclaration(declaration: SingleVariableDec): Unit = subroutineTable
    .put(declaration.name, SymbolEntry(declaration, subroutineTable.size))

  def getSymbol(name: String): SymbolEntry =
    subroutineTable.get(name)
      .orElse(argumentTable.get(name))
      .orElse(classTable.get(name))
      .orElse(classStaticTable.get(name))
      .getOrElse(throw IllegalStateException(s"Symbol '$name' doesn't exist."))

  def clearSubroutineTable(): Unit = {
    argumentTable.clear()
    subroutineTable.clear()
    argumentOffset = 0
  }

  def objectSize(): Int = classTable.size

  def setArgumentOffset(offset: Int) = argumentOffset = offset
}
