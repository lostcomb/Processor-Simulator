; Define 0
LDC r15 #0
; Define 1
LDC r3 #1
; Define 2
LDC r4 #2

; Define array A
; Set A[0] = 5
LDC r1 #0 ; Index to array
LDC r2 #5 ; Const Value
STM r1 r2

; Set A[1] = 12
ADD r1 r1 r3
LDC r2 #12
STM r1 r2

; Set A[2] = 3
ADD r1 r1 r3
LDC r2 #3
STM r1 r2

; Set A[3] = 10
ADD r1 r1 r3
LDC r2 #10
STM r1 r2

; Set A[4] = 23
ADD r1 r1 r3
LDC r2 #23
STM r1 r2

; Set A[5] = 2
ADD r1 r1 r3
LDC r2 #2
STM r1 r2

; Set A[6] = 0
ADD r1 r1 r3
LDC r2 #0
STM r1 r2

; Set A[7] = 12
ADD r1 r1 r3
LDC r2 #12
STM r1 r2

; Set A[8] = 3
ADD r1 r1 r3
LDC r2 #3
STM r1 r2

; Set A[9] = 100
ADD r1 r1 r3
LDC r2 #100
STM r1 r2

; Set A[10] = 2
ADD r1 r1 r3
LDC r2 #2
STM r1 r2

; Set A[11] = 212
ADD r1 r1 r3
LDC r2 #212
STM r1 r2

; Define Bubble Sort
; Define i = 0
LDC r1 #0
; Define size = 12
LDC r5 #12

:outer_loop_start
SUB r6 r5 r4
CGT r6 r6 r1
BEZ :outer_loop_end r6

; Set j = size - 1
SUB r2 r5 r3

:inner_loop_start
ADD r7 r1 r3 ; i + 1
CGT r8 r2 r7
CEQ r9 r2 r7
OR  r8 r8 r9
BEZ :inner_loop_end r8

; Inner Loop Body
SUB r6 r2 r3 ; r6 = j - 1
LDM r7 r6 ; r7 = MEM[j - 1]
LDM r8 r2 ; r8 = MEM[j]
CGT r9 r7 r8
BEZ :inner_loop_end r9
; If statements
ADD r10 r8 r15 ; r10 = MEM[j] + 0
STM r2 r7 ; MEM[j] = MEM[j - 1]
STM r6 r10 ; MEM[j - 1] = MEM[j]

:inner_loop_end


LDC r7 :inner_loop_start
JMP r7

LDC r6 :outer_loop_start
JMP r6
