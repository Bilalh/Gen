#!/bin/bash
set -o nounset
set -e
server="$1"

pushd "${server}"
set -x
rsync -avh --links --progress --compress --stats "${server}":sampling/ ./ --exclude="results_*"
set +x
popd