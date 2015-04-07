#/bin/bash
set -o errexit
set -o nounset

find . -name "*.prof" -delete
find . -name "*.hi" -delete
find . -name "*.o" -delete
find . -name "*.hi-boot" -delete
find . -name "*.o-boot" -delete
rm -rf dist .cabal-sandbox cabal.sandbox.config
