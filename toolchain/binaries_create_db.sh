#!/bin/bash
set -o nounset

export OUR="$( cd "$( dirname "$0" )" && pwd )";
pushd "${SAVED_BINARIES}"

"$OUR/binaries_init_db.sh" && \
parallel -j1  "$OUR/binaries_add_csv_to_db.py info.sqlite {} {//}" ::: $(find . -type f -name 'data.csv')

popd