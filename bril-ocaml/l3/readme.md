# CS 6120 L3

Kenneth Li (kql3)

## Overview

I implemented one-pass local/global trivial dead code elimination by iterating backwards through the instruction list and local value numbering that does common subexpression elimination (modulo commutativity), copy propagation, constant propagation, and constant folding. The tests mentioned below demonstrate these optimizations.

## Testing

In the `bril-ocaml/test/optimize` directory, there are a series of Turnt test cases that show the optimizations at work. Commenting out specific optimizations in the `local_` or `global_optimizations` lists in `optimize.ml` can help see what each optimization does. 
