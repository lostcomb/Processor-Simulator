# ISA Reference #

Arithmetic:

  * **ADD Rd, Ri, Rj**  `0001 dddd iiii jjjj 0000 0000 0000 0000` Add two signed integers.
  * **SUB Rd, Ri, Rj**  `0010 dddd iiii jjjj 0000 0000 0000 0000` Subtract two signed integers.
  * **MUL Rd, Ri, Rj**  `0011 dddd iiii jjjj 0000 0000 0000 0000` Multiply two signed integers.
  * **DIV Rd, Ri, Rj**  `0100 dddd iiii jjjj 0000 0000 0000 0000` Divide two signed integers.

Logic:

  * **AND Rd, Ri, Rj**  `0101 dddd iiii jjjj 0000 0000 0000 0000` Bit-wise AND two signed integers.
  * **OR  Rd, Ri, Rj**  `0110 dddd iiii jjjj 0000 0000 0000 0000` Bit-wise OR two signed integers.
  * **XOR Rd, Ri, Rj**  `0111 dddd iiii jjjj 0000 0000 0000 0000` Bit-wise XOR two signed integers.
  * **NOT Rd, Ri, Rj**  `1000 dddd iiii jjjj 0000 0000 0000 0000` Bit-wise NOT two signed integers.

Control FLow:

  * **JMP #O**          `1001 0000 0000 0000 oooo oooo oooo oooo` Unconditional jump to PC + #O.
  * **BEQ #O, Ri, Rj**  `1010 0000 iiii jjjj oooo oooo oooo oooo` Branch to PC + #O if Ri == Rj.
  * **BGT #O, Ri, Rj**  `1011 0000 iiii jjjj oooo oooo oooo oooo` Branch to PC + #O if Ri > Rj.
  * **BEZ #O, Ri**      `1100 0000 iiii 0000 oooo oooo oooo oooo` Branch to PC + #O if Ri == 0.

Load/Store:

  * **LDC Rd, #C**      `1101 dddd 0000 0000 cccc cccc cccc cccc` Load constant C into Rd.
  * **LDM Rd, MEM[Ri]** `1110 dddd iiii 0000 0000 0000 0000 0000` Load word at memory location Ri into Rd.
  * **STM MEM[Rd], Ri** `1111 dddd iiii 0000 0000 0000 0000 0000` Store word in Rj to memory location Ri.

Misc:

  * **NOP**             `0000 0000 0000 0000 0000 0000 0000 0000` No operation.

Where:
  * *Rd*     = Destination Register,
  * *Ri, Rj* = Operand Registers,
  * *C*      = Constant,
  * *O*      = Offset,
  * *MEM*    = Memory

Assembly Code:

  * Registers available: R0-R15.
  * O, C are integer constants.
  * :label where label has no spaces is a reference point in the program.
  * ; is the start of a single line comment.
  * One instruction per line.
