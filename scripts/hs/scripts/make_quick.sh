#/bin/bash
set -o errexit
set -o nounset

export USE_CORES=${CORES:-8}
cabal build "$@" -j"${USE_CORES}"
cabal copy # install in ${BIN_DIR}


