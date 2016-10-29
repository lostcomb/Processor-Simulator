:main
LDC r2 #0
LDC r3 #2
ADD r3 r3 r1
STM r3 r2
LDC r4 #3
ADD r1 r1 r4
LDC r5 #1
ADD r6 r5 r1
LDC r4 :l0
STM r6 r4
LDC r5 :setVar
JMP r5
:l0
LDM r4 r1
LDC r5 #3
SUB r1 r1 r5
LDC r7 #6
LDC r8 #3
ADD r8 r8 r1
STM r8 r7
LDC r9 #12
LDC r10 #3
ADD r10 r10 r1
STM r10 r9
LDC r2 :_end
JMP r2
:setVar
LDC r2 #9
LDC r3 #6
ADD r3 r3 r1
STM r3 r2
LDC r2 #1
ADD r2 r2 r1
LDM r2 r2
JMP r2
:_end
HALT
