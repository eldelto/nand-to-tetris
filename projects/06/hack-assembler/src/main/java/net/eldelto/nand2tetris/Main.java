package net.eldelto.nand2tetris;

import java.io.IOException;
import java.nio.file.Paths;

public class Main {
    public static void main(String[] args) throws IOException {
        for (final String filename : args) {
            final var inputPath = Paths.get(filename);
            final var outputFilename = inputPath.getFileName().toString().replace(".asm", ".hack");
            final var outputPath = inputPath.getParent().resolve(outputFilename);
            Assembler.assemble(inputPath, outputPath);
        }
    }
}
