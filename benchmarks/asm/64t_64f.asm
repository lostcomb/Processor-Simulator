:main
  LDC r1 #200 ; Define number of iterations.
  LDC r2 #0 ; i
  LDC r3 #0 ; sum

  LDC r14 #0 ; Define const 0.
  LDC r15 #1 ; Define const 1.
  LDC r13 #64 ; Define const 64.

:loop_start
  CGT r4 r1 r2
  BEZ r4 :loop_end

  ; if
  AND r5 r2 r13
  CEQ r6 r5 r14
  BEZ r6 :else

  :then
    ADD r3 r3 r15
  :else

  ADD r2 r2 r15
  LDC r4 :loop_start
  JMP r4

:loop_end
  HALT
