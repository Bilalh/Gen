#!/bin/bash
set -o nounset
OUR="$( cd "$( dirname "$0" )" && pwd )";
export OUR

cores=${CORES:-"$(parallel --number-of-cores)"};

# shellcheck disable=SC2016
parallel -j"${cores}" --keep-order \
	--rpl '{fmt} $Global::use{"File::Basename"} ||= eval "use File::Basename; 1;"; $_ = dirname($_);$_=sprintf("%-80s",$_)' \
	--tagstring '{fmt}' \
	'cat {//}/summary/hittingSet; echo "" ' \
	:::: <(find . -type d -name 'fastest*' | sort -n ) > hittingSet

function process(){
	base="$1"
	cat "${base}"/summary/resultSet "${base}"/summary/meta

	essence="$(sqlite3 "${base}/results.db" 'Select essence from metadata')"
	mode="$(sqlite3 "${base}/results.db" 'Select mode from metadata')"
	ef="${EF:-"$HOME"/repos/essence-refinements/_current}"
	json="${ef}/${essence}/${essence}_${mode}.json"
	db="${base}/results.db"

	[ -f "${json}" ] && "${OUR}/kinds.py" "${db}" "${json}"
	echo ""
}
export -f process

function process2(){
	base="$1"
	cat "${base}"/summary/resultSet2 "${base}"/summary/meta2

	essence="$(sqlite3 "${base}/results.db" 'Select essence from metadata')"
	mode="$(sqlite3 "${base}/results.db" 'Select mode from metadata')"
	ef="${EF:-"$HOME"/repos/essence-refinements/_current}"
	json="${ef}/${essence}/${essence}_${mode}.json"
	db="${base}/results.db"

	echo ""
}
export -f process2

# http://unix.stackexchange.com/questions/11856/sort-but-keep-header-line-at-the-top
# print the header (the first line of input)
# and then run the specified command on the body (the rest of the input)
# use it in a pipeline, e.g. ps | body grep somepattern
body() {
    IFS= read -r header
    printf '%s\n' "$header"
    "$@"
}

# shellcheck disable=SC2016
parallel -j"${cores}" --keep-order  \
	--rpl '{fmt} $Global::use{"File::Basename"} ||= eval "use File::Basename; 1;"; $_ = dirname($_);$_=sprintf("%-80s",$_)' \
	--tagstring '{fmt}' \
	'process {//}' \
	:::: <(find . -type d -name 'fastest*' | sort -n ) > resultSet

# shellcheck disable=SC2016
parallel -j"${cores}" --keep-order  \
	--rpl '{fmt} $Global::use{"File::Basename"} ||= eval "use File::Basename; 1;"; $_ = dirname($_);$_=sprintf("%-80s",$_)' \
	--tagstring '{fmt}' \
	'process2 {//}' \
	:::: <(find . -type d -name 'fastest*' | sort -n) > resultSet2

parallel -j"${cores}" --keep-order  \
	' ([ {#} -eq 1 ]  && cat {} ) || tail -n1 {}' \
	:::: <(find . -type f -name 'summary.csv' \( -not -path '*/r/*' \)) \
	| LC_ALL=C body sort -nk1 > all.csv

parallel -j"${cores}" --keep-order  \
	' ([ {#} -eq 1 ]  && cat {} ) || sed 1d {}' \
	:::: <(find . -type f -name 'models.csv' \( -not -path '*/r/*' \)) \
	| LC_ALL=C body sort -n -k2,2 -k8,8  -k3,3 -k4,4 -k5,5 -k1,1 > all_models.csv

