package net.eldelto.nand2tetris;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AInstructionTest {
    @ParameterizedTest
    @MethodSource("provideTestInstructions")
    void translate(final String instruction, final String expected) {
        // Setup
        final var symbolTable = new SymbolTable();

        // Execute
        final var aInstruction = new AInstruction(instruction, 0, symbolTable);
        final var result = aInstruction.translate();

        // Assert
        assertEquals(expected, result);
    }

    private static Stream<Arguments> provideTestInstructions() {
        return Stream.of(Arguments.of("@100", "0000000001100100"), Arguments.of("@i", "0000000000010000"));
    }
}