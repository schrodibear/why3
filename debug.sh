#!/bin/bash

DIR=$(dirname $(readlink -f $0))

WHERE=$(ocamlopt -where)

SCRIPT="
load_printer ${WHERE}/nums.cma
load_printer ${WHERE/ocaml/zarith}/zarith.cma
"
INCLUDES=$(find ${DIR} -name '*.cmo' -printf '%h\n' | sort -u | sed -e "s/^/-I /g")
rlwrap -P "$SCRIPT" ocamldebug ${DIR}/bin/why3${1}.byte $@
