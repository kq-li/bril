# CS 6120 L7

Kenneth Li (kql3)

## Overview

I implemented loop-invariant code motion using the algorithms outlined in lecture, taking advantage of the reaching definitions and live variable analyses implemented in L4. One tricky thing was that equivalent instructions in different locations were considered equal, so I had to augment the analyses with location data for each instruction (block name and index within block). 

## Testing

I used the Bril benchmark suite to debug the implementation, ensuring that `.out` files were preserved. I then wrote a script (`test.sh`) to compare the `.prof` files of the original benchmarks and their LICM-optimized counterparts. Here was the output:

```
diff -x '*.toml' ../test/l7/check-primes.prof ../../benchmarks/check-primes.prof
1c1
< total_dyn_inst: 7091
---
> total_dyn_inst: 8468

diff -x '*.toml' ../test/l7/fizz-buzz.prof ../../benchmarks/fizz-buzz.prof
1c1
< total_dyn_inst: 2929
---
> total_dyn_inst: 3652

diff -x '*.toml' ../test/l7/loopfact.prof ../../benchmarks/loopfact.prof
1c1
< total_dyn_inst: 101
---
> total_dyn_inst: 116

diff -x '*.toml' ../test/l7/orders.prof ../../benchmarks/orders.prof
1c1
< total_dyn_inst: 4928
---
> total_dyn_inst: 5352

diff -x '*.toml' ../test/l7/quadratic.prof ../../benchmarks/quadratic.prof
1c1
< total_dyn_inst: 577
---
> total_dyn_inst: 785

diff -x '*.toml' ../test/l7/sum-sq-diff.prof ../../benchmarks/sum-sq-diff.prof
1c1
< total_dyn_inst: 2641
---
> total_dyn_inst: 3039
```

So we can see that the number of executed instructions is significantly reduced by this optimization. 

All benchmarks not shown had no difference in the `.prof` files. There are three possible explanations for this:
1. there were just no loops
2. the loops were already well-optimized
3. the benchmark used an unsupported extension (memory/float for `eight-queens`, `fib`, `mat-mul`, `sieve`, and `sqrt`)
