# ISA Reference #

Arithmetic:

  * *ADD Rd, Ri, Rj*
    * Binary representation: `0001 dddd iiii jjjj 0000 0000 0000 0000`
    * Add two signed integers.
  * *SUB Rd, Ri, Rj*
    * Binary representation: `0010 dddd iiii jjjj 0000 0000 0000 0000`
    * Subtract two signed integers.
  * *MUL Rd, Ri, Rj*
    * Binary representation: `0011 dddd iiii jjjj 0000 0000 0000 0000`
    * Multiply two signed integers.
  * *DIV Rd, Ri, Rj*
    * Binary representation: `0100 dddd iiii jjjj 0000 0000 0000 0000`
    * Divide two signed integers.

Logic:

  * *AND Rd, Ri, Rj*
    * Binary representation: `0101 dddd iiii jjjj 0000 0000 0000 0000`
    * Bit-wise AND two signed integers.
  * *OR  Rd, Ri, Rj*
    * Binary representation: `0110 dddd iiii jjjj 0000 0000 0000 0000`
    * Bit-wise OR two signed integers.
  * *NOT Rd, Ri*
    * Binary representation: `0111 dddd iiii 0000 0000 0000 0000 0000`
    * Bit-wise NOT a signed integer.

Control FLow:

  * *JMP Ri*
    * Binary representation: `1000 0000 iiii 0000 0000 0000 0000 0000`
    * Unconditional jump, PC = Ri.
  * *BEZ #O, Ri*
    * Binary representation: `1001 0000 iiii 0000 oooo oooo oooo oooo`
    * Branch, PC = #O if Ri == 0.

Comparison:

  * *CEQ Rd, Ri, Rj*
    * Binary representation: `1010 dddd iiii jjjj 0000 0000 0000 0000`
    * Rd = 1 if Ri == Rj, Rd = 0 otherwise
  * *CGT Rd, Ri, Rj*
    * Binary representation: `1011 dddd iiii jjjj 0000 0000 0000 0000`
    * Rd = 1 if Ri < Rj, Rd = 0 otherwise.

Load/Store:

  * *LDC Rd, #C*
    * Binary representation: `1100 dddd 0000 0000 cccc cccc cccc cccc`
    * Load constant C into Rd.
  * *LDM Rd, MEM[Ri]*
    * Binary representation: `1101 dddd iiii 0000 0000 0000 0000 0000`
    * Load word at memory location Ri into Rd.
  * *STM MEM[Ri], Rj*
    * Binary representation: `1110 0000 iiii jjjj 0000 0000 0000 0000`
    * Store word in Rj to memory location Ri.

Misc:

  * *NOP*
    * Binary representation: `0000 0000 0000 0000 0000 0000 0000 0000`
    * No operation.
  * *HALT*
    * Binary representation: `1111 0000 0000 0000 0000 0000 0000 0000`
    * Halt execution. This is a pseudo instruction for use with the simulator
      only.

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
