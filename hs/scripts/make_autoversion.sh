#!/bin/bash
# Create Build_autoversion.hs that setup.hs makes
# Useful to make a archive that does not require git
set -o nounset
set -e

cat << EOF > Build_autoversion.hs
module Build_autoversion where
import Prelude(String)
autoVersion :: String
autoVersion = "$(git log -1 --format='%H (%cD)')"
EOF
