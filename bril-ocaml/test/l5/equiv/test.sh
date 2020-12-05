#! /bin/bash

diff <(bril2json < "$1" | brili "${@:2}") <(bril2json < "$1" | dune exec ../../../l5/main.exe roundtrip | brili "${@:2}")
