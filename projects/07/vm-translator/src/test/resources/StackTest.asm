// push 17
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
// push 17
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
// eq
@SP
M=M-1
@SP
A=M
D=M
@SP
A=M-1
D=M-D
M=-1
@CONTINUE_2
D;JEQ
@SP
A=M-1
M=0
(CONTINUE_2)
// push 17
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
// push 16
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
// eq
@SP
M=M-1
@SP
A=M
D=M
@SP
A=M-1
D=M-D
M=-1
@CONTINUE_5
D;JEQ
@SP
A=M-1
M=0
(CONTINUE_5)
// push 16
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
// push 17
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
// eq
@SP
M=M-1
@SP
A=M
D=M
@SP
A=M-1
D=M-D
M=-1
@CONTINUE_8
D;JEQ
@SP
A=M-1
M=0
(CONTINUE_8)
// push 892
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
// push 891
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
// lt
@SP
M=M-1
@SP
A=M
D=M
@SP
A=M-1
D=M-D
M=-1
@CONTINUE_11
D;JLT
@SP
A=M-1
M=0
(CONTINUE_11)
// push 891
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
// push 892
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
// lt
@SP
M=M-1
@SP
A=M
D=M
@SP
A=M-1
D=M-D
M=-1
@CONTINUE_14
D;JLT
@SP
A=M-1
M=0
(CONTINUE_14)
// push 891
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
// push 891
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
// lt
@SP
M=M-1
@SP
A=M
D=M
@SP
A=M-1
D=M-D
M=-1
@CONTINUE_17
D;JLT
@SP
A=M-1
M=0
(CONTINUE_17)
// push 32767
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
// push 32766
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
// gt
@SP
M=M-1
@SP
A=M
D=M
@SP
A=M-1
D=M-D
M=-1
@CONTINUE_20
D;JGT
@SP
A=M-1
M=0
(CONTINUE_20)
// push 32766
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
// push 32767
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
// gt
@SP
M=M-1
@SP
A=M
D=M
@SP
A=M-1
D=M-D
M=-1
@CONTINUE_23
D;JGT
@SP
A=M-1
M=0
(CONTINUE_23)
// push 32766
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
// push 32766
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
// gt
@SP
M=M-1
@SP
A=M
D=M
@SP
A=M-1
D=M-D
M=-1
@CONTINUE_26
D;JGT
@SP
A=M-1
M=0
(CONTINUE_26)
// push 57
@57
D=A
@SP
A=M
M=D
@SP
M=M+1
// push 31
@31
D=A
@SP
A=M
M=D
@SP
M=M+1
// push 53
@53
D=A
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
// push 112
@112
D=A
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
// neg
@SP
A=M-1
D=M
D=D-1
D=!D
M=D
// and
@SP
M=M-1
@SP
A=M
D=M
@SP
A=M-1
D=D&M
M=D
// push 82
@82
D=A
@SP
A=M
M=D
@SP
M=M+1
// or
@SP
M=M-1
@SP
A=M
D=M
@SP
A=M-1
D=D|M
M=D
// not
@SP
A=M-1
D=M
D=!D
M=D
