package net.eldelto.nand2tetris;

import java.util.HashMap;
import java.util.Optional;

public class CInstruction extends Instruction {
    private static final HashMap<String, Integer> DEST_TABLE = new HashMap<>();
    private static final HashMap<String, Integer> COMP_TABLE = new HashMap<>();
    private static final HashMap<String, Integer> JUMP_TABLE = new HashMap<>();

    static {
        DEST_TABLE.put("0", 0);
        DEST_TABLE.put("M", 1);
        DEST_TABLE.put("D", 2);
        DEST_TABLE.put("MD", 3);
        DEST_TABLE.put("A", 4);
        DEST_TABLE.put("AM", 5);
        DEST_TABLE.put("AD", 6);
        DEST_TABLE.put("AMD", 7);

        COMP_TABLE.put("0", 0b101010);
        COMP_TABLE.put("1", 0b111111);
        COMP_TABLE.put("-1", 0b111010);
        COMP_TABLE.put("D", 0b001100);
        COMP_TABLE.put("A", 0b110000);
        COMP_TABLE.put("!D", 0b001101);
        COMP_TABLE.put("!A", 0b110001);
        COMP_TABLE.put("-D", 0b001111);
        COMP_TABLE.put("-A", 0b110011);
        COMP_TABLE.put("D+1", 0b011111);
        COMP_TABLE.put("A+1", 0b110111);
        COMP_TABLE.put("D-1", 0b001110);
        COMP_TABLE.put("A-1", 0b110010);
        COMP_TABLE.put("D+A", 0b000010);
        COMP_TABLE.put("D-A", 0b010011);
        COMP_TABLE.put("A-D", 0b000111);
        COMP_TABLE.put("D&A", 0b000000);
        COMP_TABLE.put("D|A", 0b010101);

        JUMP_TABLE.put("0", 0);
        JUMP_TABLE.put("jgt", 1);
        JUMP_TABLE.put("jeq", 2);
        JUMP_TABLE.put("jge", 3);
        JUMP_TABLE.put("jlt", 4);
        JUMP_TABLE.put("jne", 5);
        JUMP_TABLE.put("jle", 6);
        JUMP_TABLE.put("jmp", 7);
    }

    public CInstruction(String rawInstruction, int lineNumber, SymbolTable symbolTable) {
        super(rawInstruction, lineNumber, symbolTable);
    }

    /**
     * Translates a C-instruction into binary code.
     * <p>
     * MD=A-1;JGT
     * MD = destination
     * A-1 = comparison
     * JGT = jump
     * <p>
     * D;JMP
     * D = comparison
     * JMP = jump
     *
     * @return The binary code of the instruction.
     */
    @Override
    public String translate() {
        String[] tmpSplit = rawInstruction.split(";", 2);
        Optional<String> jumpPart = Optional.empty();
        if (tmpSplit.length == 2) jumpPart = Optional.of(tmpSplit[1]);

        tmpSplit = tmpSplit[0].split("=", 2);
        Optional<String> destinationPart = Optional.empty();
        String comparisonPart;
        if (tmpSplit.length == 2) {
            destinationPart = Optional.of(tmpSplit[0]);
            comparisonPart = tmpSplit[1];
        } else {
            comparisonPart = tmpSplit[0];
        }

        final var destinationInstruction = translateDestination(destinationPart);
        final var comparisonInstruction = translateComparison(comparisonPart);
        final var jumpInstruction = translateJump(jumpPart);

        return intTo16BitBinary((7 << 13) | destinationInstruction | comparisonInstruction | jumpInstruction);
    }

    private int translateDestination(Optional<String> destinationPart) {
        if (destinationPart.isEmpty()) return 0;

        var address = DEST_TABLE.get(destinationPart.get());
        if (address == null)
            throw new IllegalArgumentException(destinationPart.get() + " is not a valid destination directive");

        return (address << 3);
    }


    private int translateComparison(String comparisonPart) {
        var aBit = 0;
        if (comparisonPart.contains("M")) {
            aBit = 1;
            comparisonPart = comparisonPart.replace("M", "A");
        }

        var address = COMP_TABLE.get(comparisonPart);
        if (address == null)
            throw new IllegalArgumentException(comparisonPart + " is not a valid comparison directive");

        return (aBit << 12) | (address << 6);
    }

    private int translateJump(Optional<String> jumpPart) {
        if (jumpPart.isEmpty()) return 0;

        var address = JUMP_TABLE.get(jumpPart.get().toLowerCase());
        if (address == null) throw new IllegalArgumentException(jumpPart.get() + " is not a valid jump directive");

        return address;
    }
}