LDC r1 #0
:main
LDC r2 #3
LDC r3 #8
ADD r3 r3 r1
STM r3 r2
LDC r2 :_end
JMP r2
:_end
HALT
