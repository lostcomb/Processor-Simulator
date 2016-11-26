LDC r1 #0
:main
LDC r2 #1
LDC r3 #4
LDC r4 #0
MUL r3 r3 r4
LDC r4 #8
ADD r4 r4 r1
ADD r4 r4 r3
STM r4 r2
LDC r2 #2
LDC r3 #4
LDC r4 #1
MUL r3 r3 r4
LDC r4 #8
ADD r4 r4 r1
ADD r4 r4 r3
STM r4 r2
LDC r2 #3
LDC r3 #4
LDC r4 #2
MUL r3 r3 r4
LDC r4 #8
ADD r4 r4 r1
ADD r4 r4 r3
STM r4 r2
LDC r2 #4
LDC r3 #4
LDC r4 #3
MUL r3 r3 r4
LDC r4 #8
ADD r4 r4 r1
ADD r4 r4 r3
STM r4 r2
LDC r2 #5
LDC r3 #4
LDC r4 #4
MUL r3 r3 r4
LDC r4 #8
ADD r4 r4 r1
ADD r4 r4 r3
STM r4 r2
LDC r2 :_end
JMP r2
:_end
HALT
