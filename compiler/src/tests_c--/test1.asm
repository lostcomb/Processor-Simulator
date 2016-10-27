LDC r1 #0 ; sp = 0
;LDC r2 :main ; r2 = 4
;JMP r2    ; jump to 4; main
:main
LDC r2 #0 ; r2 = 0; get i mem loc
LDC r3 #2 ; r3 = 2
ADD r4 r3 r1 ; r4 = 2 + sp = 2; set i to 0
STM r4 r2 ; MEM[2] = 0

:loop_start
LDM r2 r4 ; r2 = MEM[r11 = 2] = 0
LDC r3 #1 ; r3 = 6
CGT r11 r2 r3 ; r4 = r2 > r3 = 0
LDM r5 r4 ; r5 = MEM[2] = 0
LDC r6 #1 ; r6 = 6
CEQ r7 r5 r6 ; r7 = r5 == r6
OR r8 r11 r7 ; r8 = r11 | r7
NOT r9 r8 ; r9 = ¬r8
BEZ :loop_end r9 ; if r9 == 0 jump to 23

; loop body
LDM r2 r4
LDC r3 #1
ADD r12 r2 r3
STM r4 r12

LDC r10 :loop_start
JMP r10
:loop_end
;LDC r2 #25
;JMP r2
