package net.eldelto.nand2tetris;

import java.util.HashMap;
import java.util.Optional;

public class SymbolTable {
    private final HashMap<String, Integer> symbolMap = new HashMap<>();
    private int nextFreeAddress = 16;

    {
        symbolMap.put("SP", 0);
        symbolMap.put("LCL", 1);
        symbolMap.put("ARG", 2);
        symbolMap.put("THIS", 3);
        symbolMap.put("THAT", 4);
        symbolMap.put("R0", 0);
        symbolMap.put("R1", 1);
        symbolMap.put("R2", 2);
        symbolMap.put("R3", 3);
        symbolMap.put("R4", 4);
        symbolMap.put("R5", 5);
        symbolMap.put("R6", 6);
        symbolMap.put("R7", 7);
        symbolMap.put("R8", 8);
        symbolMap.put("R9", 9);
        symbolMap.put("R10", 10);
        symbolMap.put("R11", 11);
        symbolMap.put("R12", 12);
        symbolMap.put("R13", 13);
        symbolMap.put("R14", 14);
        symbolMap.put("R15", 15);
        symbolMap.put("SCREEN", 16384);
        symbolMap.put("KBD", 24576);
    }

    public Optional<Integer> lookupAddress(String symbol) {
        return Optional.ofNullable(symbolMap.get(symbol));
    }

    public void addSymbol(String symbol, int address) {
        validateSymbol(symbol);
        symbolMap.put(symbol, address);
    }

    public int allocateVariable(String symbol) {
        validateSymbol(symbol);
        int address = nextFreeAddress;
        nextFreeAddress++;
        symbolMap.put(symbol, address);

        return address;
    }

    private void validateSymbol(String symbol) {
        if (symbolMap.containsKey(symbol)) {
            throw new IllegalStateException("Symbol " + symbol + " is already defined");
        }
    }
}
