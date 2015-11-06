#!/bin/bash
set -o nounset
set -e

if [ $# -ne 2 ]; then
	echo "$0 sever id_to_exclude"
	exit 0
fi

server="$1"
num="$2"


case $num in
''|*[!0-9]*)
	echo "$0 sever id_to_exclude"
	exit 1
	;;
esac


pushd "${server}"
set -x
rsync -avh --links --progress --compress --stats "${server}":sampling/ ./ --exclude="results_*" --exclude="*%${num}*"
set +x
popd
