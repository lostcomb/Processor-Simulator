LDC r1 #0
:main
LDC r2 #1
BEZ r2 :l1
LDC r2 #1
LDC r3 #8
ADD r3 r3 r1
STM r3 r2
LDC r2 :l2
JMP r2
:l1
LDC r2 #2
LDC r3 #8
ADD r3 r3 r1
STM r3 r2
:l2
LDC r2 #0
BEZ r2 :l3
LDC r2 #1
LDC r3 #12
ADD r3 r3 r1
STM r3 r2
LDC r2 :l4
JMP r2
:l3
LDC r2 #2
LDC r3 #12
ADD r3 r3 r1
STM r3 r2
:l4
LDC r2 :_end
JMP r2
:_end
HALT
