; Define a
LDC r1 #10
; Define b
LDC r2 #8
; Define 0
LDC r3 #0

; While loop
:loop_start
; Loop condition
CEQ r4 r2 r3
NOT r4 r4
BEZ :loop_end r4

; Loop body
; Set t = b
ADD r5 r2 r3
; Set b = a % b
DIV r6 r1 r2
MUL r6 r6 r2
SUB r2 r1 r6
; Set a = t
ADD r1 r5 r3

LDC r4 :loop_start
JMP r4
:loop_end

; Set all registers to 0 bar the result Register
LDC r2  #0
LDC r3  #0
LDC r4  #0
LDC r5  #0
LDC r6  #0
LDC r7  #0
LDC r8  #0
LDC r9  #0
LDC r10 #0
LDC r11 #0
LDC r12 #0
LDC r13 #0
LDC r14 #0
LDC r15 #0
HALT
