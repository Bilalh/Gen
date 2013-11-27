#!/bin/bash
# Bilal Syed Hussain
set -o nounset
Dir="$( cd "$( dirname "$0" )" && pwd )";
${Dir}/perModel.sh "$@" 2>&1
