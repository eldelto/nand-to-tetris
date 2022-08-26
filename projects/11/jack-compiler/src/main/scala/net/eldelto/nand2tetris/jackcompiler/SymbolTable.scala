package net.eldelto.nand2tetris.jackcompiler

import scala.collection.mutable.HashMap

case class SymbolEntry(declaration: SingleVariableDec, index: Int)

class SymbolTable {
  var className = ""
  private val classTable = HashMap[String, SymbolEntry]()
  private val subroutineTable = HashMap[String, SymbolEntry]()

  def addClassDeclaration(declaration: SingleVariableDec): Unit =
    classTable.put(declaration.name, SymbolEntry(declaration, classTable.size))

  def addSubroutineDeclaration(declaration: SingleVariableDec): Unit = subroutineTable
    .put(declaration.name, SymbolEntry(declaration, subroutineTable.size))

  def getSymbol(name: String): SymbolEntry =
    subroutineTable.get(name).orElse(classTable.get(name)).getOrElse(throw IllegalStateException(s"Symbol '$name' doesn't exist."))

  def clearSubroutineTable(): Unit = subroutineTable.clear()
}
