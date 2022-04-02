package net.eldelto.nand2tetris.vmtranslator

import java.nio.file._

@main def main(filename: String): Unit = {
  Translator
    .translate(Path.of(filename))
    .swap
    .map(err => println("Error: " + err))
}
