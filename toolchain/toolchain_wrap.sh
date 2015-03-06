#!/bin/bash
set -o nounset

if [ ! -z "${NULL_TOOLCHAIN_OUTPUT:-}" ]; then
	"$PARAM_GEN_SCRIPTS/toolchain/toolchain.py" "$@" &> /dev/null
elif [  ! -z "${REDIRECT_TOOLCHAIN_OUTPUT:-}" ]; then
	"$PARAM_GEN_SCRIPTS/toolchain/toolchain.py" "$@" &>"$REDIRECT_TOOLCHAIN_OUTPUT"
else
	"$PARAM_GEN_SCRIPTS/toolchain/toolchain.py" "$@"
fi


