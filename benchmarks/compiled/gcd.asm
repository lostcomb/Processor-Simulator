:main
LDC r2 #12
ADD r1 r1 r2
LDC r3 #4
ADD r4 r3 r1
LDC r2 :l1
STM r4 r2
LDC r5 #10
ADD r4 r4 r3
STM r4 r5
LDC r6 #13
ADD r4 r4 r3
STM r4 r6
LDC r3 :gcd
JMP r3
:l1
LDM r2 r1
LDC r3 #12
ADD r1 r1 r3
LDC r3 #8
ADD r3 r3 r1
STM r3 r2
LDC r2 :_end
JMP r2
:gcd
:l2
LDC r2 #12
ADD r2 r2 r1
LDM r2 r2
LDC r3 #0
CEQ r2 r2 r3
NOT r2 r2
LDC r3 #1
AND r2 r2 r3
BEZ r2 :l3
LDC r2 #12
ADD r2 r2 r1
LDM r2 r2
LDC r3 #16
ADD r3 r3 r1
STM r3 r2
LDC r2 #20
ADD r1 r1 r2
LDC r3 #4
ADD r4 r3 r1
LDC r2 :l4
STM r4 r2
LDC r5 #8
ADD r5 r5 r1
LDM r5 r5
ADD r4 r4 r3
STM r4 r5
LDC r6 #12
ADD r6 r6 r1
LDM r6 r6
ADD r4 r4 r3
STM r4 r6
LDC r3 :mod
JMP r3
:l4
LDM r2 r1
LDC r3 #20
ADD r1 r1 r3
LDC r3 #12
ADD r3 r3 r1
STM r3 r2
LDC r2 #16
ADD r2 r2 r1
LDM r2 r2
LDC r3 #8
ADD r3 r3 r1
STM r3 r2
LDC r2 :l2
JMP r2
:l3
LDC r2 #8
ADD r2 r2 r1
LDM r2 r2
LDC r3 #0
ADD r3 r3 r1
STM r3 r2
LDC r3 #4
ADD r3 r3 r1
LDM r3 r3
JMP r3
LDC r2 #4
ADD r2 r2 r1
LDM r2 r2
JMP r2
:mod
LDC r2 #8
ADD r2 r2 r1
LDM r2 r2
LDC r3 #12
ADD r3 r3 r1
LDM r3 r3
DIV r2 r2 r3
LDC r4 #16
ADD r4 r4 r1
STM r4 r2
LDC r2 #8
ADD r2 r2 r1
LDM r2 r2
LDC r3 #16
ADD r3 r3 r1
LDM r3 r3
LDC r4 #12
ADD r4 r4 r1
LDM r4 r4
MUL r3 r3 r4
ADD r2 r2 r3
LDC r5 #0
ADD r5 r5 r1
STM r5 r2
LDC r5 #4
ADD r5 r5 r1
LDM r5 r5
JMP r5
LDC r2 #4
ADD r2 r2 r1
LDM r2 r2
JMP r2
:_end
HALT
