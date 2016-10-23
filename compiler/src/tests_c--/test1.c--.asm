[ LDC r1 #55
, LDC r34 #4
, JMP r34
, LDC r3 #1 //main
, LDC r4 #2
, ADD r5 r4 r3
, ADD r6 r5 r1
, LDC r7 #0// load i
, STM r6 r7// store i
, LDC r8 #0
, LDC r9 #2
, ADD r10 r9 r8
, ADD r11 r10 r1
, LDM r12 r11// load val of i into r12
, LDC r13 #6 // load const 6
, CEQ r14 r12 r13 // compare equality
, LDC r15 #0
, LDC r16 #2
, ADD r17 r16 r15
, ADD r18 r17 r1
, LDM r19 r18 // load val of i into r19
, LDC r20 #6 // load const 6
, CGT r21 r19 r20 // compare gt
, OR r22 r14 r21 // or gt and eq
, NOT r23 r22 // not or
, BEZ #41 r23
, LDC r24 #0
, LDC r25 #2
, ADD r26 r25 r24
, ADD r27 r26 r1
, LDC r28 #0
, LDC r29 #2
, ADD r30 r29 r28
, ADD r31 r30 r1
, LDM r32 r31
, LDC r33 #1
, ADD r34 r32 r33
, STM r27 r34
, LDC r35 #10
, JMP r35
, LDC r24 #0
, LDC r25 #1
, ADD r26 r25 r24
, ADD r27 r26 r1
, LDC r28 #0
, STM r27 r28
, LDC r29 #0
, LDC r30 #0
, ADD r31 r30 r29
, ADD r32 r31 r1
, LDM r33 r32
, BEZ #54 r33
, JMP r33
]
