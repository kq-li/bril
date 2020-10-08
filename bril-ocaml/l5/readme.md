# CS 6120 L5

Kenneth Li (kql3)

## Overview

I implemented several useful utilities related to dominance and a static single assignment converter. The dominance utilities are in the `Dominance` module, grouped into `Dominance.Lists` and `Dominance.Sets` depending on the desired output data structure. The SSA converter module has functions `Ssa.to_ssa` and `Ssa.from_ssa` that convert `Bril.Func.t`s to and from SSA form. The CLI `dune exec ./main.exe` takes an argument `ssa|roundtrip` specifying whether to output SSA or convert to SSA and back again.

Overall, I encountered some difficulty implementing the `to_ssa` algorithm without mutability (ugh), so this assignment took about 8-10 hours.

## Testing

In the `bril-ocaml/test/l5` directory, there is a `is_ssa` subdirectory that contains tests verifying that the converter correctly outputs SSA, and an `equiv` subdirectory that ensures that roundtripping the programs does not change the behavior.
