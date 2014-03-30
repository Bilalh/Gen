#!/bin/bash
# mimics wc -l *.minion-solution but a lot faster (seconds instead of hours for a 1tb file)
set -o nounset

if ! ( ls *.minion-solution >/dev/null ); then
	echo "no minion-solution files"
	exit 1
fi

function count_lines(){
	sol=$1
	head_file="$(mktemp /tmp/mm.XXXXXX)"
	if [ "$(uname)" == "Darwin" ]; then
		statf='-f %z'
	else
		statf='--printf %s'
	fi
	head -n1 ${sol} > ${head_file}
	oneline="`stat ${statf} ${head_file}`"
	alllines="`stat ${statf} ${sol}`"
	#echo ${oneline} ${alllines}

	printf "%15d %s\n" $((alllines/oneline)) ${sol}

	rm $head_file
}
export -f count_lines

count_file=${COUNT_FILE:-solutions.counts}

parallel --keep-order  "count_lines {}" ::: *.minion-solution | tee ${count_file} 
