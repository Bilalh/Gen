#!/bin/bash
set -o nounset
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )";


function echoing(){
    echo "$@"
    "$@"
}

_dir="$1$(date +%F_%H-%M_%s)/"
mkdir -p "$_dir"
dir="$(python -c 'import os,sys; print(os.path.realpath(sys.argv[1]))'  "${_dir}")"

"$SCRIPT_DIR/save_version.sh" "$dir/_versions"

cd "$dir"

repos_dir="$(dirname "$(which savilerow)")/../"


cp "$(which conjure)" .
cp "$(which savilerow)" .
cp "$(which minion)" .
cp "$repos_dir/savilerow/savilerow.jar" .
cp -r "$repos_dir/instancegen/scripts/hs/bin" ig_bin
