#!/bin/bash
set -o nounset

if [ ! -z "${NULL_TOOLCHAIN_OUTPUT:-}" ]; then
	"$$REPO_GEN/toolchain/toolchain.py" "$@" &> /dev/null
elif [  ! -z "${REDIRECT_TOOLCHAIN_OUTPUT:-}" ]; then
	"$REPO_GEN/toolchain/toolchain.py" "$@" &>"$REDIRECT_TOOLCHAIN_OUTPUT"
else
	"$REPO_GEN/toolchain/toolchain.py" "$@"
fi


