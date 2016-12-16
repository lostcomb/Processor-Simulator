:main
  LDC r1 #12
  LDC r2 #1
  LDC r4 :start
  LDC r15 #1

:start
  CGT r3 r1 r15
  BEZ r3 :end

  MUL r2 r2 r1
  SUB r1 r1 r15

  JMP r4

:end
  LDC r1  #0
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
