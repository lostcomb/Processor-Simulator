LDC r1 #0
:main
LDC r2 #12
ADD r1 r1 r2
LDC r3 #4
ADD r3 r3 r1
LDC r2 :l1
STM r3 r2
LDC r3 :getX
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
:getX
LDC r2 #17
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
:_end
HALT
