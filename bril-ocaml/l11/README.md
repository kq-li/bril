# CS 6120 L11

Kenneth Li (kql3)

## Overview

I implemented a simple tracing optimizer by first using a new `--trace` flag on `brili` to run a Bril program until a print statement is encountered or until the end of the program, printing all executed instructions. Then, using this printed trace, I synthesize a speculative instruction block with branches replaced by guards and jumps ignored and insert it into the proper place in the original program. I also implemented a simple check to move guard statements earlier -- either right after the most recent definition of its argument or right after the most recent `speculate` statement.

## Testing

I used the example from the lesson and a simple factorial example to show the behavior of the tracer. The `../test/l11/test.sh` script computes a diff between the outputs (and instruction counts) for the original program and the newly traced program, and is used to produce the `.out` files for `turnt`.
