#!/bin/bash
set -o nounset
Dir="$( cd "$( dirname "$0" )" && pwd )";

case "$USER" in
"bh246")
    NUM_JOBS=${NUM_JOBS:-32} ;;
"azureuser")
    NUM_JOBS=${NUM_JOBS:-8} ;;
*)
    NUM_JOBS=${NUM_JOBS:-4} ;;
esac
export NUM_JOBS

function save_versions(){
    fp="$1/$(date +%F_%H-%M_%s)"

	if ( git rev-parse --is-inside-work-tree ); then
		echo "repo(git) version" >> "${fp}.versions"
		git log -1 --format="%H" >> "${fp}.versions"
		git describe --always    >> "${fp}.versions"
        echo "" >> "${fp}.versions"
	fi

	echo "conjure">> "${fp}.versions"
	conjure 2>&1 | grep Version						 >> "${fp}.versions"
    echo "" >> "${fp}.versions"
	minion 2>&1	 | egrep 'HG version|Minion Version' >> "${fp}.versions"
    echo "" >> "${fp}.versions"
	savilerow	 | grep Version         			 >> "${fp}.versions"
    echo "" >> "${fp}.versions"

	echo "##OTHER##"  >> "${fp}.versions"
	echo "" >> "${fp}.versions"


    local commands=(python python3 pip sqlite3 git hg perl parallel ruby )
	for prog in "${commands[@]}"; do
		echo "$prog version:" >> "${fp}.versions"
		$prog --version  2>&1 | cat >> "${fp}.versions"
		echo "" >> "${fp}.versions"
	done

	# Java just had to be different
	echo "java version:" >> "${fp}.versions"
	java -version  2>&1 | cat >> "${fp}.versions"

}

save_versions data

"$Dir/make_minions.sh" &&
"$Dir/make_tree.sh"    &&
"$Dir/make_csv.sh"
