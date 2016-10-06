LDC r2 #200
LDC r3 #128
LDC r7 #12
ADD r1 r2 r3
:label_1 ;Comment
SUB r8 r7 r2
JMP :label_1
;Comment
JMP r1
