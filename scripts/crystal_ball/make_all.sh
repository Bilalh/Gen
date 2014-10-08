#!/bin/bash
set -o nounset
Dir="$( cd "$( dirname "$0" )" && pwd )";

"$Dir/make_minions.sh"
"$Dir/make_tree.sh"
"$Dir/make_csv.sh"
