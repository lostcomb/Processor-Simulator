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

  LDC r15 #0  ; Const 0.
  LDC r14 #4  ; Const 4.
  LDC r13 #8  ; Const 8.
  LDC r12 #12 ; Const 12.

  LDC r1 #60 ; Set stack pointer to 60.
  STM r1 r15 ; push 0
  LDC r3 #44
  ADD r2 r1 r14
  STM r2 r3 ; push 44
  ADD r2 r1 r13
  LDC r3 :end
  STM r2 r3 ; push ret_addr

:quick_sort
  LDM r2 r1 ; r2 = p
  ADD r3 r1 r14
  LDM r4 r3 ; r4 = r
  CGT r5 r4 r2
  BEZ r5 :qs_else

:qs_then
  LDC r3 :partition
  JMP r3
  :ret_1
  ; Push values to stack
  ADD r1 r1 r12 ; allocate stack frame
  ADD r5 r3 r14 ; q + 1
  STM r1 r5 ; push q + 1
  ADD r5 r1 r14 ; sp + 4
  STM r5 r4 ; push r
  LDC r5 :qs_ret
  ADD r6 r1 r13
  STM r6 r5 ; push ret_addr
  ADD r1 r1 r12 ; allocate stack frame
  STM r1 r2 ; push p
  SUB r5 r3 r14 ; q - 1
  ADD r6 r1 r14 ; sp + 4
  STM r6 r5 ; push q - 1
  LDC r5 :ret_2
  ADD r6 r1 r13
  STM r6 r5 ; push ret_addr

  ; call quick_sort
  LDC r7 :quick_sort
  JMP r7
  :ret_2
  ; call quick_sort
  LDC r8 :quick_sort
  JMP r8

:qs_else

:qs_ret
  ADD r2 r1 r13
  LDM r3 r2 ; r3 = ret_addr
  SUB r1 r1 r12 ; remove stack frame
  JMP r3

:partition ; r2 = p, r4 = r, r3 = ret_val ; registers - r5 - r11
  LDM r5 r4 ; x = list[r]
  SUB r11 r2 r14 ; i = p - 1

:part_loop_start
  CGT r6 r4 r2
  BEZ r6 :part_loop_end

  LDM r7 r2 ; list[j]
  CGT r10 r7 r5 ; list[j] > x
  BEZ r10 :part_else

  :part_if
    ADD r11 r11 r14 ; i = i + 1
    LDM r6 r11 ; list[i]
    LDM r7 r2 ; list[j]
    STM r11 r7 ; list[i] = list[j]
    STM r2 r6 ; list[j] = old list[i]
  :part_else


  ADD r2 r2 r14 ; j = j + 1
  LDC r9 :part_loop_start
  JMP r9

:part_loop_end
  ADD r3 r11 r14 ; i + 1
  LDM r6 r3 ; list[i + 1]
  LDM r7 r4 ; list[r]
  STM r3 r7 ; list[i + 1] = list[r]
  STM r4 r6 ; list[r] = old list[i + 1]

  LDC r5 :ret_1
  JMP r5

:end
  HALT
