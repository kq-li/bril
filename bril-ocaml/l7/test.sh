#! /bin/bash

cd $(dirname "$0")
diff ../test/l7 ../../benchmarks -x *.toml | sed 's/diff -x/\ndiff -x/g' | sed '1d'
