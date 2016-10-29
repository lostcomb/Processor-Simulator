:main
LDC r2 #0
LDC r3 #2
ADD r3 r3 r1
STM r3 r2
LDC r4 #0
LDC r5 #3
ADD r5 r5 r1
STM r5 r4
:l0
LDC r6 #3
ADD r6 r6 r1
LDM r6 r6
LDC r7 #5
CGT r8 r7 r6
BEZ r8 :l1
LDC r2 #3
ADD r2 r2 r1
LDM r2 r2
LDC r3 #1
ADD r4 r2 r3
LDC r5 #2
ADD r5 r5 r1
STM r5 r4
LDC r6 #3
ADD r6 r6 r1
LDM r6 r6
LDC r7 #1
ADD r8 r6 r7
LDC r9 #3
ADD r9 r9 r1
STM r9 r8
LDC r8 :l0
JMP r8
:l1
LDC r2 :_end
JMP r2
:_end
HALT
