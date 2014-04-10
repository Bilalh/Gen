#!/bin/bash
# Find the the hitting of each Entry in Info.db
# and place the results in ${2}

set -o nounset
export our_dir="$( cd "$( dirname "$0" )" && pwd )";
echo $our_dir

dir=${1}
res=${2}
mkdir -p ${res}

export selector=$( cat <<EOF
	Select distinct 'mset(
		' || group_concat( ( '{ ' || eprimes || ' }' ), ',
		')  ||  '
	) $ ~~~'  as A from ParamsData where quality < 1;
EOF
)

export smallest_query=$( cat <<EOF
Select DISTINCT eprimes From  ParamsData
Where eprimes_count = (Select min(eprimes_count) From ParamsData);
EOF
)


export Command=$( cat <<EOF
	echo "~~~";
	put={output_dir};
	put="$res/{essence}-{method}-{run_no}";
	echo \$put;
	mkdir -p \$put;

	sql --no-headers -s ',' sqlite3:///{base_dir}/{output_dir}/results.db "${smallest_query}" > \$put/smallest

	# echo \$put/sets.param;
	echo "language Essence 1.3" > \$put/sets.param;

	printf "letting numSets be "  >> \$put/sets.param;
	sql --no-headers -s ',' sqlite3:///{base_dir}/{output_dir}/results.db \
		"Select distinct count(eprimes) From ParamsData Where quality < 1;"\
			>> \$put/sets.param;

	printf "letting numModels be "  >> \$put/sets.param;
	sql --no-headers -s ',' sqlite3:///\$db \
		"Select num_models From essences Where essence='{essence}';"  >> \$put/sets.param;

	printf "letting sets be "  >> \$put/sets.param;
	sql --no-headers -s ',' sqlite3:///{base_dir}/{output_dir}/results.db "$selector" >> \$put/sets.param;

	pushd \$put > /dev/null;

	mkdir -p run;
	cd run;
	export __convenience_fp=$our_dir/../misc/convenience.sh;
	source $our_dir/../misc/convenience.sh;
	export NUM_JOBS=1;
	refine_run  $our_dir/hittingSetMsetOpt/*.essence ::: $our_dir/hittingSetMsetOpt/*/*.eprime ::: ../*.param &> refine_run.out;
	cp *.solution ../hittingSet.solution;

	do_gent "{base_dir}/{output_dir}/results.db";

	popd > /dev/null;
	echo "~---~";
EOF
)

function do_gent(){
	db=$1
	if [ -f ../hittingSet.solution ]; then
		egrep -o "\{.*\}" ../hittingSet.solution > ../hittingSet;
		${our_dir}/gent_idea.py "$db" "$(cat ../hittingSet)" | sed -e "s/'//g" > ../gentSet
	else
		echo "NOTHING" >  ../hittingSet;
		echo "NOTHING" > ../gentSet;
	fi
}

export -f do_gent


function process(){
	export db=$1
	#method,essence,total_timeout,models_timeout,races,chain_length,radius_as_percentage,
	#influence_radius,num_points,point_selector,run_no,output_dir,mode,num_models
	sql -s ',' sqlite3:///$db "Select *, '$(dirname $(dirname "$db"))' as base_dir from everything;" \
		| parallel --tagstring "{2}%{1}%{11}"  --tag -j1 --keep-order  --header , --colsep , "$Command"
}
export -f process

find "$dir" -type f -name 'Info.db'  |   parallel --keep-order  process

pushd ${res}
parallel --keep-order  --tagstring "{/.}" 'cat {}/hittingSet' ::: */ > _hittingSet_all.txt
parallel --keep-order  --tagstring "{/.}" 'cat {}/gentSet' ::: */ > _gentSet_all.txt
parallel --keep-order  --tagstring "{/.}" 'cat {}/smallest' ::: */ > _smallest_all.txt
popd
