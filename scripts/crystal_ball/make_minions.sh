#!/bin/bash
set -o nounset

for dir in data/*; do
    if [ "$dir" == "data/__" ]; then
        continue
    fi

    pushd "$dir"
    parallel --resume --tag -j"${NUM_JOBS:-0}" --joblog minions/_.jobs\
        "savilerow -in-eprime {1} -in-param {2} -out-minion minions/{1/.}_{2/.}.minion \
        -out-aux minions/{1/.}_{2/.}.aux" \
        ::: eprimes/*.eprime ::: params/*.param

    popd
done
