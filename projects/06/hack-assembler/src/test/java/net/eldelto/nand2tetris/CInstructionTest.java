package net.eldelto.nand2tetris;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CInstructionTest {
    @ParameterizedTest
    @MethodSource("provideTestInstructions")
    void translate(final String instruction, final String expected) {
        // Setup
        final var symbolTable = new SymbolTable();

        // Execute
        final var cInstruction = new CInstruction(instruction, 0, symbolTable);
        final var result = cInstruction.translate();

        // Assert
        assertEquals(expected, result);
    }

    private static Stream<Arguments> provideTestInstructions() {
        return Stream.of(Arguments.of("M=1", "1110111111001000"), Arguments.of("D=D-A", "1110010011010000"), Arguments.of("D;JGT", "1110001100000001"));
    }
}