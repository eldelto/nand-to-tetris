package net.eldelto.nand2tetris;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AssemblerTest {
    private static final String ASM_SUFFIX = ".asm";
    private static final String BINARY_SUFFIX = ".hack";

    @ParameterizedTest
    @ValueSource(strings = {"Max", "Rect", "Pong"})
    void assemble(final String testFile) throws Exception {
        // Setup
        final var inputPath = getTestFilePath(testFile + ASM_SUFFIX);
        final var outputPath = createTempFile(testFile + BINARY_SUFFIX);
        final var comparisonPath = getTestFilePath(testFile + BINARY_SUFFIX);

        // Execute
        Assembler.assemble(inputPath, outputPath);

        // Assert
        final var outputData = Files.readString(outputPath);
        final var comparisonData = Files.readString(comparisonPath);

        assertEquals(comparisonData, outputData, "Should produce the same binary content");
    }

    private Path getTestFilePath(String name) {
        return Paths.get(getClass().getClassLoader().getResource(name).getPath());
    }

    private Path createTempFile(String name) throws IOException {
        return Files.createTempFile(name, null);
    }
}