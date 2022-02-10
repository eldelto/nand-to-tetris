// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

(MAIN)
	@SCREEN		// Init screen pointer.
	D=A
	@screen-pointer
	M=D

	@KBD		// Read from keyboard.
	D=M

	@WHITE-COLOR	// Fill screen white if no key is pressed...
	D;JEQ

	@BLACK-COLOR	// ...otherwise black.
	0;JMP

(WHITE-COLOR)
	@color
	M=0
	@FILL-SCREEN
	0;JMP

(BLACK-COLOR)
	@color
	M=-1
	@FILL-SCREEN
	0;JMP

(FILL-SCREEN)
	@color
	D=M

	@screen-pointer
	A=M
	M=D	// Set pixels to color.

	@screen-pointer
	M=M+1
	D=M
	@24576
	D=A-D
	@MAIN
	D;JLE

	@FILL-SCREEN
	0;JMP

