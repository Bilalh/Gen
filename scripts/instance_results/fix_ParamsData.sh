#!/bin/bash
set -o nounset
OUR="$( cd "$( dirname "$0" )" && pwd )";
export OUR


parallel --tagstring {//} "sqlite3 {} <'${OUR}/fix_ParamsData.sql' "  \
	:::: <(find . -type f -name results.db)
