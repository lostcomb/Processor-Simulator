## Compilation: ##

1. `cd` into the project directory.
2. Run `cabal sandbox init`.
3. Run `cabal install`.

## Execution: ##

1. `cd` into the project directory.
2. Run `cabal run aca-processor-simulator -- simulate [ options ]` substituting
   `[ options ]` for the desired options. To get a list of possible options,
   run `cabal run aca-processor-simulator -- simulate --help`.
