#!/bin/bash
set -o nounset
Dir="$( cd "$( dirname "$0" )" && pwd )";

for dir in $(find data/ -maxdepth 1 -mindepth 1 -type d -not -name __) ; do

    pushd "$dir"
    mkdir -p minions

    parallel --resume --tag -j"${NUM_JOBS:-4}" --joblog minions/_.jobs\
        "savilerow -in-eprime {1} -in-param {2} -out-minion minions/{1/.}_{2/.}.minion \
        -out-aux minions/{1/.}_{2/.}.aux" \
        ::: eprimes/*.eprime ::: params/*.param

    popd
done
