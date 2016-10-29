:main
LDC r2 #5
ADD r1 r1 r2
LDC r3 #1
ADD r4 r3 r1
LDC r2 :l0
STM r4 r2
LDC r2 #3
SUB r2 r1 r2
ADD r4 r4 r3
STM r4 r2
LDC r3 :setArr
JMP r3
:l0
LDM r2 r1
LDC r3 #5
SUB r1 r1 r3
LDC r2 :_end
JMP r2
:setArr
LDC r2 #7
LDC r3 #1
LDC r4 #2
ADD r4 r4 r1
ADD r4 r4 r3
STM r4 r2
LDC r2 #1
ADD r2 r2 r1
LDM r2 r2
JMP r2
:_end
HALT
