# CS 6120 L4

Kenneth Li (kql3)

## Overview

I implemented a generalized dataflow analysis framework as a functor on modules (specifying the domain, meet, top, and transfer function). This made it really easy to create new dataflow analyses. For this task, I've implemented analysis of defined variables (the basic one from the second video), live variables, reaching definitions, and constant propagation/folding. The main executable takes a command line argument to specify the analysis (i.e. `dune exec ./main.exe reaching` -- can be `defined|live|reaching|constant`).

## Testing

In the `bril-ocaml/test/l4` directory, there are a series of Turnt test cases that show the various analyses at work. 
