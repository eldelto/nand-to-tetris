// push 111
@111
D=A
@SP
A=M
M=D
@SP
M=M+1
// push 333
@333
D=A
@SP
A=M
M=D
@SP
M=M+1
// push 888
@888
D=A
@SP
A=M
M=D
@SP
M=M+1
// pop Static
@STATIC_8
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
// pop Static
@STATIC_3
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
// pop Static
@STATIC_1
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
// pop Static
@STATIC_3
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
// pop Static
@STATIC_1
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
// sub
@SP
M=M-1
A=M
D=M
@SP
A=M-1
D=M-D
M=D
// pop Static
@STATIC_8
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
A=M
D=M
@SP
A=M-1
D=M+D
M=D
