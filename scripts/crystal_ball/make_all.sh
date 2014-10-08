#!/bin/bash
set -o nounset
Dir="$( cd "$( dirname "$0" )" && pwd )";

case "$USER" in
"bh246")
    NUM_JOBS=32 ;;
"azureuser")
    NUM_JOBS=8 ;;
*)
    NUM_JOBS=4 ;;
esac
export NUM_JOBS

"$Dir/make_minions.sh" &&
"$Dir/make_tree.sh"    &&
"$Dir/make_csv.sh"

echo $NUM_JOBS
