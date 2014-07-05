#!/bin/bash
set -x
set -o nounset
set -e

base=${1}

mkdir -p "${base}/summary"

find "${base}"  -name "results.json" -exec cat {} +  | \
  jq '.gErrors, .gErrors_timeout, .gErrors_no_use | .[] | .erroed as $e | .result_dir as $r | .results[$e] | {"status": .status_, "dir" : $r, "cmd" : .cmd[1:4] } ' -c \
	> "${base}/summary/info.summary"

IFS=,
find "${base}"  -name "results.json" -exec cat {} +  | \
  jq '.gBase as $b |  .gErrors, .gErrors_timeout, .gErrors_no_use | .[] | .result_dir+","+$b  ' -r | while read dir dirpath;  do
	cp -r "${dirpath}/${dir/_/}" "${base}/summary"
done
set +x
