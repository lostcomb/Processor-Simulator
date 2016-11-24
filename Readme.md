# Advanced Computer Architecture Processor Simulator #

## TODO: Compile instructions. ##

## TODO: ##

* Implement sub pipeline in the EU.
* Implement re-order buffer.
* Implement out-of-order scheduling.
* Implement branch prediction.
* Implement multiple execution units.
* Implement reservation stations.

# ISA Reference: #

| Assembly Representation | Binary Representation                     | Description                                                                   |
| ----------------------- | ----------------------------------------- | ----------------------------------------------------------------------------- |
| ADD Rd, Ri, Rj          | `0001 dddd iiii jjjj 0000 0000 0000 0000` | Add two signed integers.                                                      |
| SUB Rd, Ri, Rj          | `0010 dddd iiii jjjj 0000 0000 0000 0000` | Subtract two signed integers.                                                 |
| MUL Rd, Ri, Rj          | `0011 dddd iiii jjjj 0000 0000 0000 0000` | Multiply two signed integers.                                                 |
| DIV Rd, Ri, Rj          | `0100 dddd iiii jjjj 0000 0000 0000 0000` | Divide two signed integers.                                                   |
| AND Rd, Ri, Rj          | `0101 dddd iiii jjjj 0000 0000 0000 0000` | Bit-wise AND two signed integers.                                             |
| OR  Rd, Ri, Rj          | `0110 dddd iiii jjjj 0000 0000 0000 0000` | Bit-wise OR two signed integers.                                              |
| NOT Rd, Ri              | `0111 dddd iiii 0000 0000 0000 0000 0000` | Bit-wise NOT a signed integer. i.e. complement each bit.                      |
| JMP Ri                  | `1000 0000 iiii 0000 0000 0000 0000 0000` | Unconditional jump, PC = Ri.                                                  |
| BEZ Ri, #O              | `1001 0000 iiii 0000 oooo oooo oooo oooo` | Branch, PC = #O if Ri == 0.                                                   |
| CEQ Rd, Ri, Rj          | `1010 dddd iiii jjjj 0000 0000 0000 0000` | Rd = 1 if Ri == Rj, Rd = 0 otherwise.                                         |
| CGT Rd, Ri, Rj          | `1011 dddd iiii jjjj 0000 0000 0000 0000` | Rd = 1 if Ri < Rj, Rd = 0 otherwise.                                          |
| LDC Rd, #C              | `1100 dddd 0000 0000 cccc cccc cccc cccc` | Load constant C into Rd.                                                      |
| LDM Rd, MEM[Ri]         | `1101 dddd iiii 0000 0000 0000 0000 0000` | Load word at memory location Ri into Rd.                                      |
| STM MEM[Ri], Rj         | `1110 0000 iiii jjjj 0000 0000 0000 0000` | Store word in Rj to memory location Ri.                                       |
| NOP                     | `0000 0000 0000 0000 0000 0000 0000 0000` | No operation.                                                                 |
| HALT                    | `1111 0000 0000 0000 0000 0000 0000 0000` | Halt execution. This is a pseudo instruction for use with the simulator only. |

Where:

| Destination Register | Operand Registers | Constant | Offset | Memory | Program Counter |
| -------------------- | ----------------- | -------- | ------ | ------ | --------------- |
| Rd                   | Ri, Rj            | C        | O      | MEM    | PC              |

Available registers: `R0-R15`. `R0` is the program counter and can only be modified with the `JMP` and `BEZ` instructions.
