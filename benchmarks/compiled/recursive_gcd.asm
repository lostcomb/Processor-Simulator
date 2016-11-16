:main
LDC r2 #3
ADD r1 r1 r2
LDC r3 #1
ADD r4 r3 r1
LDC r2 :l1
STM r4 r2
LDC r5 #10
ADD r4 r4 r3
STM r4 r5
LDC r6 #13
ADD r4 r4 r3
STM r4 r6
LDC r3 :gcd
JMP r3
:l1
LDM r2 r1
LDC r3 #3
ADD r1 r1 r3
LDC r7 #2
ADD r7 r7 r1
STM r7 r2
LDC r2 :_end
JMP r2
:gcd
LDC r2 #2
ADD r2 r2 r1
LDM r2 r2
LDC r3 #3
ADD r3 r3 r1
LDM r3 r3
CEQ r2 r2 r3
BEZ r2 :l2
LDC r2 #2
ADD r2 r2 r1
LDM r2 r2
LDC r3 #0
ADD r3 r3 r1
STM r3 r2
LDC r3 #1
ADD r3 r3 r1
LDM r3 r3
JMP r3
LDC r2 :l3
JMP r2
:l2
LDC r2 #2
ADD r2 r2 r1
LDM r2 r2
LDC r3 #3
ADD r3 r3 r1
LDM r3 r3
CGT r2 r2 r3
BEZ r2 :l4
LDC r2 #4
ADD r1 r1 r2
LDC r3 #1
ADD r4 r3 r1
LDC r2 :l6
STM r4 r2
LDC r5 #2
ADD r5 r5 r1
LDM r5 r5
LDC r6 #3
ADD r6 r6 r1
LDM r6 r6
ADD r5 r5 r6
ADD r4 r4 r3
STM r4 r5
LDC r7 #3
ADD r7 r7 r1
LDM r7 r7
ADD r4 r4 r3
STM r4 r7
LDC r3 :gcd
JMP r3
:l6
LDM r2 r1
LDC r3 #4
ADD r1 r1 r3
LDC r3 #0
ADD r3 r3 r1
STM r3 r2
LDC r3 #1
ADD r3 r3 r1
LDM r3 r3
JMP r3
LDC r2 :l5
JMP r2
:l4
LDC r2 #4
ADD r1 r1 r2
LDC r3 #1
ADD r4 r3 r1
LDC r2 :l7
STM r4 r2
LDC r5 #2
ADD r5 r5 r1
LDM r5 r5
ADD r4 r4 r3
STM r4 r5
LDC r6 #3
ADD r6 r6 r1
LDM r6 r6
LDC r7 #2
ADD r7 r7 r1
LDM r7 r7
ADD r6 r6 r7
ADD r4 r4 r3
STM r4 r6
LDC r3 :gcd
JMP r3
:l7
LDM r2 r1
LDC r3 #4
ADD r1 r1 r3
LDC r3 #0
ADD r3 r3 r1
STM r3 r2
LDC r3 #1
ADD r3 r3 r1
LDM r3 r3
JMP r3
:l5
:l3
LDC r2 #1
ADD r2 r2 r1
LDM r2 r2
JMP r2
:_end
HALT
