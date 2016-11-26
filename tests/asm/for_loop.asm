LDC r1 #0
:main
LDC r2 #0
LDC r3 #28
ADD r3 r3 r1
STM r3 r2
:l1
LDC r2 #28
ADD r2 r2 r1
LDM r2 r2
LDC r3 #5
CGT r2 r3 r2
BEZ r2 :l2
LDC r2 #28
ADD r2 r2 r1
LDM r2 r2
LDC r3 #3
MUL r2 r2 r3
LDC r3 #4
LDC r4 #28
ADD r4 r4 r1
LDM r4 r4
MUL r3 r3 r4
LDC r4 #8
ADD r4 r4 r1
ADD r4 r4 r3
STM r4 r2
LDC r2 #28
ADD r2 r2 r1
LDM r2 r2
LDC r3 #1
ADD r2 r2 r3
LDC r3 #28
ADD r3 r3 r1
STM r3 r2
LDC r2 :l1
JMP r2
:l2
LDC r2 :_end
JMP r2
:_end
HALT
