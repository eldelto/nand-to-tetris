package net.eldelto.nand2tetris;

public abstract class Instruction {
    protected final String rawInstruction;
    protected final int lineNumber;
    protected final SymbolTable symbolTable;

    public Instruction(String rawInstruction, int lineNumber, SymbolTable symbolTable) {
        this.rawInstruction = rawInstruction.trim();
        this.lineNumber = lineNumber;
        this.symbolTable = symbolTable;
    }

    public abstract String translate();

    protected String intTo16BitBinary(int value) {
        return Integer.toBinaryString((1 << 16) | value).substring(1);
    }
}
