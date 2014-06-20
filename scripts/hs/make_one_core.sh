#/bin/bash
set -o errexit
set -o nounset

cabal build -j1
cabal copy # install in ${BIN_DIR}


