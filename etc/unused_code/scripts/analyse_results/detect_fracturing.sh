#!/bin/bash
set -o nounset

#method,essence,total_timeout,models_timeout,races,chain_length,radius_as_percentage,
#influence_radius,num_points,point_selector,run_no,output_dir,mode,num_models

dir=${1}
args=${2:-}

export Command=$( cat <<EOF
	# echo {output_dir}
	$PARAM_GEN_SCRIPTS/analyse_results/detect_fracturing.py ${args}  {base_dir}/{output_dir}/results.db
EOF
)

function process(){
	db=$1
	sql -s ',' sqlite3:///$db "Select *, '$(dirname $(dirname $db))' as base_dir from everything;" \
		| parallel -j1 --tagstring "{2}%{1}" --keep-order  --header , --colsep , "$Command"
}
export -f process

find $dir -type f -name 'Info.db'  |   parallel --keep-order  process

