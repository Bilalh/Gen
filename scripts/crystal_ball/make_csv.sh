#!/bin/bash
set -o nounset
Dir="$( cd "$( dirname "$0" )" && pwd )";

for dir in data/*; do
    if [ "$dir" == "data/__" ]; then
        continue
    fi

    pushd "$dir"
    mkdir -p csv

    parallel --resume --tag -j"${NUM_JOBS:-4}" --joblog csv/_.jobs \
        "$Dir/dumptree/dumptree.py {} csv/{/.}.csv \
        --dot csv/{/.}.dot --meta csv/{/.}.json" \
        ::: tree/*.minion-tree

    popd
done
