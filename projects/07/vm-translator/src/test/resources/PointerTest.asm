// push 3030
@3030
D=A
@SP
A=M
M=D
@SP
M=M+1
// pop Pointer
@THIS
D=A
@R13
M=D
@SP
M=M-1
@SP
A=M
D=M
@R13
A=M
M=D
// push 3040
@3040
D=A
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
@SP
A=M
D=M
@R13
A=M
M=D
// push 32
@32
D=A
@SP
A=M
M=D
@SP
M=M+1
// pop THIS
@THIS
D=M
@2
D=A+D
@R13
M=D
@SP
M=M-1
@SP
A=M
D=M
@R13
A=M
M=D
// push 46
@46
D=A
@SP
A=M
M=D
@SP
M=M+1
// pop THAT
@THAT
D=M
@6
D=A+D
@R13
M=D
@SP
M=M-1
@SP
A=M
D=M
@R13
A=M
M=D
// pop Pointer
@THIS
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
// add
@SP
M=M-1
@SP
A=M
D=M
@SP
A=M-1
D=M+D
M=D
// pop THIS
@THIS
D=M
@2
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
// sub
@SP
M=M-1
@SP
A=M
D=M
@SP
A=M-1
D=M-D
M=D
// pop THAT
@THAT
D=M
@6
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
@SP
A=M
D=M
@SP
A=M-1
D=M+D
M=D
