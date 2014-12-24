#!/bin/bash
set -o nounset

"$PARAM_GEN_SCRIPTS/toolchain/toolchain.py" "$@" &> /dev/null