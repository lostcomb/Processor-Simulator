# Advanced Computer Architecture Processor Simulator #

For the ISA reference look in: `isa_reference.txt`.

## To compile and run the simulator, assembler: ##

### To compile the simulator: ###

1. `cd` into the `simulator/src` folder.

2. Run `cabal install containers`.

3. Run `cabal install split`.

4. Run `cabal install bytestring`.

5. Run `ghc -o simulator Main.hs`.

### To run the simulator: ###

1. From the `simulator/src` folder, run `./simulator "path_to_program.o"`.
   An example of this would be: `cabal run "../benchmarks/gcd.o"`.

2. When the simulator starts, step along program execution by pressing the
   carriage return / enter key.

### To compile the assembler: ###

1. `cd` into the `assembler/src` folder.

2. Run `cabal install containers`.

3. Run `cabal install bytestring`.

4. Run `cabal install parsec`.

5. Run `ghc -o assembler Main.hs`.

### To run the assembler: ###

1. `cd` to the `assembler/src` folder.

2. Run `./assembler "assemble_bin" "path_to_assembly_code.asm" "path_to_destination.o"`.
