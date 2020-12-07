#! /bin/bash

filename="$1"
args="${@:2}"

out1=$(bril2json < "$filename" | brili --trace "$args" | dune exec ../../l11/main.exe "$filename" | brili -p "$args" 2>&1)
out2=$(bril2json < "$filename" | brili -p "$args" 2>&1)
diff <(echo $out1) <(echo $out2) || true
