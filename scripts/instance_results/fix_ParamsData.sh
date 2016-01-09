#!/bin/bash
set -o nounset
OUR="$( cd "$( dirname "$0" )" && pwd )";
export OUR

function process(){
	db="$1"
	base="$2"
	id="${base##*%}"

	if ( grep -q "^\*%${id}\*$" synced.txt ); then
		return 0
	else
		sqlite3 "${db}" <"${OUR}/fix_ParamsData.sql"
	fi
}
export -f process

parallel --tagstring {//} "process {} {//}"  \
	:::: <(find . -type f -name results.db)
