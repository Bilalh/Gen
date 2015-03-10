#!/bin/bash
set -o nounset

tbase="$1"
now="$2"
host_type="$3"

cat << EOF > "${tbase}/meta.json"
{
	 "bin_dir"   : "date/${now}"
	,"host_type" : "${host_type}"
}
EOF



