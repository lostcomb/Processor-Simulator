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
  * *XOR Rd, Ri, Rj*
    * Binary representation: `0111 dddd iiii jjjj 0000 0000 0000 0000`
    * Bit-wise XOR two signed integers.
  * *NOT Rd, Ri*
    * Binary representation: `1000 dddd iiii 0000 0000 0000 0000 0000`
    * Bit-wise NOT a signed integer.

Control FLow:

  * *JMP Ri*
    * Binary representation: `1001 0000 iiii 0000 0000 0000 0000 0000`
    * Unconditional jump to PC + Ri.
  * *BEQ #O, Ri, Rj*
    * Binary representation: `1010 0000 iiii jjjj oooo oooo oooo oooo`
    * Branch to PC + #O if Ri == Rj.
  * *BGT #O, Ri, Rj*
    * Binary representation: `1011 0000 iiii jjjj oooo oooo oooo oooo`
    * Branch to PC + #O if Ri > Rj.
  * *BEZ #O, Ri*
    * Binary representation: `1100 0000 iiii 0000 oooo oooo oooo oooo`
    * Branch to PC + #O if Ri == 0.

Load/Store:

  * *LDC Rd, #C*
    * Binary representation: `1101 dddd 0000 0000 cccc cccc cccc cccc`
    * Load constant C into Rd.
  * *LDM Rd, MEM[Ri]*
    * Binary representation: `1110 dddd iiii 0000 0000 0000 0000 0000`
    * Load word at memory location Ri into Rd.
  * *STM MEM[Ri], Rj*
    * Binary representation: `1111 0000 iiii jjjj 0000 0000 0000 0000`
    * Store word in Rj to memory location Ri.

Misc:

  * *NOP*
    * Binary representation: `0000 0000 0000 0000 0000 0000 0000 0000`
    * No operation.

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
