// pop ARG
@ARG
D=M
@1
D=A+D
@R13
M=D
@R13
A=M
D=M
@SP
A=M
M=D
@SP
M=M+1
// pop Pointer
@THAT
D=A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
// push 0
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
// pop THAT
@THAT
D=M
@0
D=A+D
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
// push 1
@1
D=A
@SP
A=M
M=D
@SP
M=M+1
// pop THAT
@THAT
D=M
@1
D=A+D
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
// pop ARG
@ARG
D=M
@0
D=A+D
@R13
M=D
@R13
A=M
D=M
@SP
A=M
M=D
@SP
M=M+1
// push 2
@2
D=A
@SP
A=M
M=D
@SP
M=M+1
// sub
@SP
M=M-1
A=M
D=M
@SP
A=M-1
D=M-D
M=D
// pop ARG
@ARG
D=M
@0
D=A+D
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
// label MAIN_LOOP_START
(MAIN_LOOP_START)
// pop ARG
@ARG
D=M
@0
D=A+D
@R13
M=D
@R13
A=M
D=M
@SP
A=M
M=D
@SP
M=M+1
// if-goto COMPUTE_ELEMENT
@SP
@SP
M=M-1
A=M
D=M
@COMPUTE_ELEMENT
D;JNE
// goto END_PROGRAM
@END_PROGRAM
0;JMP
// label COMPUTE_ELEMENT
(COMPUTE_ELEMENT)
// pop THAT
@THAT
D=M
@0
D=A+D
@R13
M=D
@R13
A=M
D=M
@SP
A=M
M=D
@SP
M=M+1
// pop THAT
@THAT
D=M
@1
D=A+D
@R13
M=D
@R13
A=M
D=M
@SP
A=M
M=D
@SP
M=M+1
// add
@SP
M=M-1
A=M
D=M
@SP
A=M-1
D=M+D
M=D
// pop THAT
@THAT
D=M
@2
D=A+D
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
// pop Pointer
@THAT
D=A
@R13
M=D
@R13
A=M
D=M
@SP
A=M
M=D
@SP
M=M+1
// push 1
@1
D=A
@SP
A=M
M=D
@SP
M=M+1
// add
@SP
M=M-1
A=M
D=M
@SP
A=M-1
D=M+D
M=D
// pop Pointer
@THAT
D=A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
// pop ARG
@ARG
D=M
@0
D=A+D
@R13
M=D
@R13
A=M
D=M
@SP
A=M
M=D
@SP
M=M+1
// push 1
@1
D=A
@SP
A=M
M=D
@SP
M=M+1
// sub
@SP
M=M-1
A=M
D=M
@SP
A=M-1
D=M-D
M=D
// pop ARG
@ARG
D=M
@0
D=A+D
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
// goto MAIN_LOOP_START
@MAIN_LOOP_START
0;JMP
// label END_PROGRAM
(END_PROGRAM)
