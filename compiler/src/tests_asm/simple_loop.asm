LDC r2 #200
LDC r3 #128
LDC r7 #12
ADD r1 r3 r7
:loop
ADD r1 r1 r7
BGT :loop r2 r1
STM r7 r1
