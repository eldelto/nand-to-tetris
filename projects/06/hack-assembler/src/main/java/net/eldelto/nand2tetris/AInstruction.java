package net.eldelto.nand2tetris;

public class AInstruction extends Instruction {
    public AInstruction(String rawInstruction, int lineNumber, SymbolTable symbolTable) {
        super(rawInstruction, lineNumber, symbolTable);
    }

    /**
     * Translates an A-instruction into binary code.
     * <p>
     * {@literal @}1230
     * {@literal @}variable
     *
     * @return The binary code of the instruction.
     */
    @Override
    public String translate() {
        if (!rawInstruction.startsWith("@"))
            throw new IllegalArgumentException(String.format("Line %d: %s is not a valid A-instruction", lineNumber, rawInstruction));
        final int address = resolveAddress(rawInstruction);
        return intTo16BitBinary(address);
    }

    private boolean isInteger(String value) {
        return value.matches("\\d+");
    }

    private int resolveAddress(String rawInstruction) {
        final String symbol = rawInstruction.substring(1);
        int address;
        if (isInteger(symbol)) {
            address = Integer.parseInt(symbol);
        } else {
            address = symbolTable.lookupAddress(symbol).orElseGet(() -> symbolTable.allocateVariable(symbol));
        }

        return address;
    }
}
