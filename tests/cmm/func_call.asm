:main
LDC r2 #3
ADD r1 r1 r2
LDC r3 #1
ADD r4 r3 r1
LDC r2 :l0
STM r4 r2
LDC r3 :const_12
JMP r3
:l0
LDM r2 r1
LDC r3 #3
SUB r1 r1 r3
LDC r5 #2
ADD r5 r5 r1
STM r5 r2
LDC r2 :_end
JMP r2
:const_12
LDC r2 #12
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
:_end
HALT
