#/bin/bash
set -o errexit
set -o nounset

export USE_CORES=${CORES:-8}
cabal build -j"${USE_CORES}"  --ghc-options="-prof -auto-all"
cabal copy # install in ${BIN_DIR}


