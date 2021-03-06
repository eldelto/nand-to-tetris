package net.eldelto.nand2tetris.jackcompiler

import scala.collection.mutable.HashMap

case class SymbolEntry(declaration: SingleVariableDec, index: Int)

class SymbolTable {
    private val classTable = HashMap[String, SymbolEntry]()
    private val subroutineTable = HashMap[String, SymbolEntry]()

    def addClassDeclaration(declaration: SingleVariableDec) = classTable.put(declaration.name, SymbolEntry(declaration, classTable.size))

    def addSubroutineDeclaration(declaration: SingleVariableDec) = subroutineTable.put(declaration.name, SymbolEntry(declaration, subroutineTable.size))

    def getSymbol(name: String) = subroutineTable.get(name).orElse(classTable.get(name)).get

    def clearSubroutineTable() = subroutineTable.clear()
}
