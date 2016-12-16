:main
  ; Set A[0] = 5
  LDC r1 #0 ; Index to array
  LDC r2 #5 ; Const Value
  STM r1 r2
  ; Set A[1] = 12
  LDC r1 #4
  LDC r2 #12
  STM r1 r2
  ; Set A[2] = 3
  LDC r1 #8
  LDC r2 #3
  STM r1 r2
  ; Set A[3] = 10
  LDC r1 #12
  LDC r2 #10
  STM r1 r2
  ; Set A[4] = 23
  LDC r1 #16
  LDC r2 #23
  STM r1 r2
  ; Set A[5] = 2
  LDC r1 #20
  LDC r2 #2
  STM r1 r2
  ; Set A[6] = 0
  LDC r1 #24
  LDC r2 #0
  STM r1 r2
  ; Set A[7] = 12
  LDC r1 #28
  LDC r2 #12
  STM r1 r2
  ; Set A[8] = 3
  LDC r1 #32
  LDC r2 #3
  STM r1 r2
  ; Set A[9] = 100
  LDC r1 #36
  LDC r2 #100
  STM r1 r2
  ; Set A[10] = 2
  LDC r1 #40
  LDC r2 #2
  STM r1 r2
  ; Set A[11] = 212
  LDC r1 #44
  LDC r2 #212
  STM r1 r2

  LDC r15 #1 ; Define const 1
  LDC r14 #4 ; Define const 4

  ; Define Bubble Sort
  LDC r1 #0 ; Define i = 0
  LDC r2 #10 ; Define size - 2 = 10

:outer_loop_start
  CGT r3 r2 r1
  BEZ r3 :outer_loop_end

  LDC r3 #11 ; Set j = size - 1 = 11

  :inner_loop_start
    ADD r4 r1 r15 ; i + 1
    CGT r5 r4 r3
    NOT r6 r5
    AND r7 r6 r15
    BEZ r7 :inner_loop_end

    ; Inner Loop Body
    MUL r8 r3 r14 ; t_j = j * 4
    LDM r4 r8 ; MEM [ j ]
    SUB r5 r8 r14 ; r5 = j * 4 - 4
    LDM r6 r5 ; MEM [ j - 1 ]
    CGT r7 r6 r4
    BEZ r7 :else

    :then
      STM r8 r6
      STM r5 r4
    :else

    SUB r3 r3 r15

    LDC r4 :inner_loop_start
    JMP r4

:inner_loop_end
  ADD r1 r1 r15
  LDC r3 :outer_loop_start
  JMP r3

:outer_loop_end
  HALT
