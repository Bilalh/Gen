#!/bin/bash
set -o nounset
cd data
parallel --resume --tag -j"${NUM_JOBS:-4}" --joblog _dot.jobs \
    "time dot -Tpdf -o {}.pdf {}" ::: $(find . -type f -name '*.dot' -size -250k)
