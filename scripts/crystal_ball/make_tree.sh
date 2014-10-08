#!/bin/bash
set -o nounset

function mk_varorder(){
    if [[ "$1" == "default"  ]]; then
        echo ""
    else
        echo "-varorder $1"
    fi
}

export -f mk_varorder

for dir in data/*; do
    if [ "$dir" == "data/__" ]; then
        continue
    fi

    pushd "$dir"
    mkdir -p tree

    parallel --resume --tag -j"${NUM_JOBS:-4}" --joblog tree/_.jobs \
        "minion {1} -quiet -noprintsols -solsout tree/{1/.}.{2}.minion-solution \
        -tableout tree/{1/.}.{2}.minion-table  -dumptree > tree/{1/.}.{2}.minion-tree \
        -nodelimit ${NODE_LIMIT:-100000}
        \$(mk_varorder {2})" \
        ::: minions/*.minion \
        ::: default sdf
        # ::: default sdf sdf-random srf srf-random ldf ldf-random random static

    popd
done
