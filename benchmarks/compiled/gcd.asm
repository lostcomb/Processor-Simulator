:main
LDC r2 #3
ADD r1 r1 r2
LDC r2 #1
ADD r2 r2 r1
LDC r2 :l1
STM r2 r2
LDC r2 #10
ADD r2 r2 r2
STM r2 r2
LDC r2 #13
ADD r2 r2 r2
STM r2 r2
LDC r2 :gcd
JMP r2
:l1
LDM r2 r1
LDC r2 #3
ADD r1 r1 r2
LDC r2 #2
ADD r2 r2 r1
STM r2 r2
LDC r2 :_end
JMP r2
:gcd
:l2
LDC r2 #3
ADD r2 r2 r1
LDM r2 r2
LDC r2 #0
CEQ r2 r2 r2
NOT r2 r2
LDC r2 #1
AND r2 r2 r2
BEZ r2 :l3
LDC r2 #3
ADD r2 r2 r1
LDM r2 r2
LDC r2 #4
ADD r2 r2 r1
STM r2 r2
LDC r2 #5
ADD r1 r1 r2
LDC r2 #1
ADD r2 r2 r1
LDC r2 :l4
STM r2 r2
LDC r2 #2
ADD r2 r2 r1
LDM r2 r2
ADD r2 r2 r2
STM r2 r2
LDC r2 #3
ADD r2 r2 r1
LDM r2 r2
ADD r2 r2 r2
STM r2 r2
LDC r2 :mod
JMP r2
:l4
LDM r2 r1
LDC r2 #5
ADD r1 r1 r2
LDC r2 #3
ADD r2 r2 r1
STM r2 r2
LDC r2 #4
ADD r2 r2 r1
LDM r2 r2
LDC r2 #2
ADD r2 r2 r1
STM r2 r2
LDC r2 :l2
JMP r2
:l3
LDC r2 #2
ADD r2 r2 r1
LDM r2 r2
LDC r2 #0
ADD r2 r2 r1
STM r2 r2
LDC r2 #1
ADD r2 r2 r1
LDM r2 r2
JMP r2
LDC r2 #1
ADD r2 r2 r1
LDM r2 r2
JMP r2
:mod
LDC r2 #2
ADD r2 r2 r1
LDM r2 r2
LDC r2 #3
ADD r2 r2 r1
LDM r2 r2
DIV r2 r2 r2
LDC r2 #4
ADD r2 r2 r1
STM r2 r2
LDC r2 #2
ADD r2 r2 r1
LDM r2 r2
LDC r2 #4
ADD r2 r2 r1
LDM r2 r2
LDC r2 #3
ADD r2 r2 r1
LDM r2 r2
MUL r2 r2 r2
ADD r2 r2 r2
LDC r2 #0
ADD r2 r2 r1
STM r2 r2
LDC r2 #1
ADD r2 r2 r1
LDM r2 r2
JMP r2
LDC r2 #1
ADD r2 r2 r1
LDM r2 r2
JMP r2
:_end
HALT
