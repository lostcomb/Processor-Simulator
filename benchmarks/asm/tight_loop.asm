:main
  LDC r1 #0 ; i
  LDC r2 #500 ; n
  LDC r3 #1 ; 1
  LDC r4 :loop_start

:loop_start
  CGT r5 r2 r1
  BEZ r5 :loop_end

  ; Loop body, run instructions with false dependencies.
  ADD r6 r2 r1
  ADD r6 r3 r7
  SUB r6 r9 r10
  ADD r6 r2 r1
  ADD r6 r3 r7
  SUB r6 r9 r10
  ADD r6 r2 r1
  ADD r6 r3 r7
  SUB r6 r9 r10
  ADD r6 r2 r1
  ADD r6 r3 r7
  SUB r6 r9 r10
  ADD r6 r2 r1
  ADD r6 r3 r7
  SUB r6 r9 r10
  ADD r6 r2 r1
  ADD r6 r3 r7
  SUB r6 r9 r10

  ADD r1 r1 r3

  JMP r4

:loop_end
  HALT
