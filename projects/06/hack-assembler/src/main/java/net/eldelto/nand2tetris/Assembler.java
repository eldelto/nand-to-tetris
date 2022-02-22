package net.eldelto.nand2tetris;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedList;
import java.util.stream.Stream;

/**
 * Translates Hack ASM code to binary instructions.
 */
public class Assembler {
    public static void assemble(final Path inputPath, final Path outputPath) throws IOException {
        final var symbolTable = new SymbolTable();
        final var instructions = new LinkedList<Instruction>();

        try (final var reader = Files.newBufferedReader(inputPath)) {
            var instructionAddress = 0;
            var line = reader.readLine();
            while (line != null) {
                line = sanitizeLine(line);
                if (line.length() > 0) {
                    var firstChar = line.charAt(0);
                    switch (firstChar) {
                        case '@':
                            instructions.add(new AInstruction(line, instructionAddress, symbolTable));
                            instructionAddress++;
                            break;
                        case '(':
                            symbolTable.addSymbol(extractSymbol(line), instructionAddress);
                            break;
                        default:
                            instructions.add(new CInstruction(line, instructionAddress, symbolTable));
                            instructionAddress++;
                    }
                }
                line = reader.readLine();
            }
        }

        final Stream<CharSequence> asmStream = instructions.stream().map(Instruction::translate);

        Files.write(outputPath, asmStream::iterator);
    }

    private static String sanitizeLine(String line) {
        return line.trim().replaceAll("//.*$", "");
    }

    private static String extractSymbol(String line) {
        return line.replaceAll("\\(|\\)", "");
    }

    /*
    - Switch between A- and C-instructions
    - A instruction
        - Starts with @
        - If address is a number we translate directly
        - If address is a symbol we look it up in the symbol table
    - C instruction
        - Parse the different parts (destination, comparison, jump)
        - Translate to the respective binary code
    - Label
        - Create an entry in the symbol table referring to the next line number
    - Pass over the in-memory representation, resolve all symbols and insert unknown symbols with a new memory address.
     */
}
