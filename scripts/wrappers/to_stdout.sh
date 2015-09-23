#!/bin/bash
# Bilal Syed Hussain
set -o nounset
Dir="$( cd "$( dirname "$0" )" && pwd )";

"$@" 2>&1