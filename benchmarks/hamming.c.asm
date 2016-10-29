:main
LDC r2 #3
ADD r1 r1 r2
LDC r3 #1
ADD r4 r3 r1
LDC r2 :l0
STM r4 r2
LDC r5 #10
ADD r4 r4 r3
STM r4 r5
LDC r3 :hamming
JMP r3
:l0
LDM r2 r1
LDC r3 #3
SUB r1 r1 r3
LDC r6 #2
ADD r6 r6 r1
STM r6 r2
LDC r2 :_end
JMP r2
:hamming
LDC r2 #0
LDC r3 #2
ADD r3 r3 r1
STM r3 r2
:l1
LDC r4 #5
ADD r4 r4 r1
LDM r4 r4
LDC r5 #0
CGT r6 r4 r5
BEZ r6 :l2
LDC r2 #8
ADD r1 r1 r2
LDC r3 #1
ADD r4 r3 r1
LDC r2 :l3
STM r4 r2
LDC r5 #5
ADD r5 r5 r1
LDM r5 r5
ADD r4 r4 r3
STM r4 r5
LDC r6 #2
ADD r4 r4 r3
STM r4 r6
LDC r3 :mod
JMP r3
:l3
LDM r2 r1
LDC r3 #8
SUB r1 r1 r3
LDC r7 #7
ADD r7 r7 r1
STM r7 r2
LDC r8 #2
ADD r8 r8 r1
LDM r8 r8
LDC r9 #7
ADD r9 r9 r1
LDM r9 r9
ADD r10 r8 r9
LDC r11 #2
ADD r11 r11 r1
STM r11 r10
LDC r12 #5
ADD r12 r12 r1
LDM r12 r12
LDC r13 #2
DIV r14 r12 r13
LDC r15 #5
ADD r15 r15 r1
STM r15 r14
LDC r6 :l1
JMP r6
:l2
LDC r2 #2
ADD r2 r2 r1
LDM r2 r2
LDC r3 #0
ADD r3 r3 r1
STM r3 r2
LDC r3 #1
ADD r3 r3 r1
LDM r3 r3
JMP r3
LDC r2 #1
ADD r2 r2 r1
LDM r2 r2
JMP r2
:mod
LDC r2 #10
ADD r2 r2 r1
LDM r2 r2
LDC r3 #11
ADD r3 r3 r1
LDM r3 r3
DIV r4 r2 r3
LDC r5 #12
ADD r5 r5 r1
STM r5 r4
LDC r6 #10
ADD r6 r6 r1
LDM r6 r6
LDC r7 #12
ADD r7 r7 r1
LDM r7 r7
LDC r8 #11
ADD r8 r8 r1
LDM r8 r8
MUL r9 r7 r8
SUB r10 r6 r9
LDC r11 #0
ADD r11 r11 r1
STM r11 r10
LDC r11 #1
ADD r11 r11 r1
LDM r11 r11
JMP r11
LDC r2 #1
ADD r2 r2 r1
LDM r2 r2
JMP r2
:_end
HALT
