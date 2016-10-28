# Advanced Computer Architecture Processor Simulator #

For the ISA reference look in: `isa_reference.txt`.

## To compile and run the simulator, assembler and compiler: ##

1. `cd` into the root directory of the project. (Contains `assembler`,
   `benchmarks`, `compiler`, `simulator` folders).

2. Run `cabal sandbox init`.

### To compile the simulator: ###

1. `cd` into the `simulator` folder.

2. Run `cabal sandbox init --sandbox="../.cabal-sandbox"`.

3. Run `cabal install`.

### To run the simulator: ###

1. From the `simulator` folder, run `cabal run "path_to_program.o"`.
   An example of this would be: `cabal run "../benchmarks/gcd.o"`.

2. When the simulator starts, step along program execution by pressing the
   carriage return / enter key.

### To compile the assembler: ###

1. `cd` into the `assembler` folder.

2. Run `cabal sandbox init --sandbox="../.cabal-sandbox"`.

3. Run `cabal install`.

### To run the assembler: ###

1. `cd` to the `assembler` folder.

2. Run `cabal run "assemble_bin" "path_to_assembly_code.asm" "path_to_destination.o"`.

### To compile the compiler: ###

1. `cd` into the `compiler` folder.

2. Run `cabal sandbox init --sandbox="../.cabal-sandbox"`.

3. Run `cabal install`.

### To run the compiler: ###

1. `cd` into the `compiler` folder.

2. Run `cabal run "generate" "path_to_input_program.cmm" "path_to_output_assembly.asm"`.
