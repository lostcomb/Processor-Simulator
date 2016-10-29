:main
LDC r2 #1
LDC r3 #3
ADD r3 r3 r1
STM r3 r2
LDC r4 #3
ADD r4 r4 r1
LDM r4 r4
BEZ r4 :l0
LDC r2 #2
LDC r3 #2
ADD r3 r3 r1
STM r3 r2
LDC r4 :l1
JMP r4
:l0
LDC r2 #5
LDC r3 #2
ADD r3 r3 r1
STM r3 r2
:l1
LDC r2 #0
LDC r3 #3
ADD r3 r3 r1
STM r3 r2
LDC r4 #3
ADD r4 r4 r1
LDM r4 r4
BEZ r4 :l2
LDC r2 #4
LDC r3 #2
ADD r3 r3 r1
STM r3 r2
LDC r4 :l3
JMP r4
:l2
LDC r2 #7
LDC r3 #2
ADD r3 r3 r1
STM r3 r2
:l3
LDC r2 :_end
JMP r2
:_end
HALT
