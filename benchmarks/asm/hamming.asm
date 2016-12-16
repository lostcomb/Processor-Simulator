:main
  LDC r1 #99 ; Define n
  LDC r2 #0 ; Define 0
  LDC r3 #2 ; Define 2

  LDC r5 #0 ; Set h = 0

:loop_start
  CGT r4 r1 r2
  BEZ r4 :loop_end

  ; Loop body
  ; Set m = n % 2
  DIV r4 r1 r3
  MUL r4 r4 r3
  SUB r4 r1 r4 ; r4 = m
  ; Set h = h + m
  ADD r5 r5 r4
  ; Set n = n / 2
  DIV r1 r1 r3

  LDC r4 :loop_start
  JMP r4

:loop_end
  ADD r1 r5 r2 ; Set r1 = h
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
