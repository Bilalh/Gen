#!/bin/bash
set -o nounset
Dir="$( cd "$( dirname "$0" )" && pwd )";

for dir in $(find data/ -maxdepth 1 -mindepth 1 -type d -not -name __) ; do

    pushd "$dir"
    mkdir -p csv

    parallel --resume --tag -j"${NUM_JOBS:-4}" --joblog csv/_.jobs \
        "$Dir/dumptree/dumptree.py {} csv/{/.}.csv \
        --dot csv/{/.}.dot --meta-csv csv/{/.}.csv-meta" \
        ::: tree/*.minion-tree

    popd
done
