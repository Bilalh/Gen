#!/bin/bash
set -o nounset

# get the repository base
Dir="$( cd "$( dirname "$0" )" && pwd )"
Script_Base="$Dir/../"


results_dir=${GENERATED_OUTPUT_DIR:-"${OUT_BASE_DIR:-.}/results-`basename $(pwd)`-${USE_MODE}"}
#"
stats_dir=${STATS_OUTPUT_DIR:-"${OUT_BASE_DIR:-.}/stats-`basename $(pwd)`-${USE_MODE}"}
#"
fastest_dir=${FASTEST_OUTPUT_DIR:-"${OUT_BASE_DIR:-.}/fastest-`basename $(pwd)`-${USE_MODE}"}
#"

echo " <<`basename $0` vars>>"
echo "pwd: `pwd`"
echo $REPOSITORY_BASE
echo "results_dir: $results_dir"
echo "stats_dir  : $stats_dir"
echo "fastest_dir: $fastest_dir"
echo " <<`basename $0` vars>>"

echo "TOTAL_TIMEOUT: $TOTAL_TIMEOUT"
echo "USE_DATE: $USE_DATE"


Essence=$(ls *.essence | head -n 1)
Essence_base=${Essence%.essence}

${Script_Base}/db/init_db.sh

param_glob=${PARAM_BASE_NAME}

param_num="`cat ${stats_dir}/${USE_DATE}.params-used | sort | uniq | wc -l | egrep -o '^ *[0-9]+' | tr -d ' '`"
eprime_num="`wc -l  $(ls ${stats_dir}/${USE_DATE}.models-used | tail -n1)  | egrep -o '^ *[0-9]+ ' | tr -d ' '`"

echo "param_num: $param_num"
echo "eprime_num $eprime_num"

while read minion_timeout total_timeout; do
	echo "INSERT OR REPLACE into Timeouts('param', 'MinionTimeout','TotalTimeout', 'timestamp')
				  Values('${PARAM_BASE_NAME}', ${minion_timeout}, ${total_timeout}, '$USE_DATE' );" \
	|  sqlite3 ${REPOSITORY_BASE}/results.db
done < `ls ${stats_dir}/${USE_DATE}.timeout-used | tail -n1`


echo "INSERT OR REPLACE into Metadata('essence')
			  Values('${Essence_base}');" \
	|  sqlite3 ${REPOSITORY_BASE}/results.db


echo "<$0> using *.info"


function doMinionTable(){
	set -x
	fp="$1"
	sr_time="${results_dir}/$2.sr-time"


	${Script_Base}/db/parse_minion_tableout.py "$fp" ${REPOSITORY_BASE}/results.db `grep real ${sr_time} | tail -n1 | sed -Ee 's/.*m([0-9]+.[0-9]+).*/\1/'`
	set +x
}

export  results_dir
export Script_Base
export REPOSITORY_BASE
export -f doMinionTable


parallel --tagstring "{/.}"  'echo $(doMinionTable {} {/.})' ::: ${results_dir}/*${param_glob}*.minion-table


# I could not get traping SIGTERM to work in perModel.sh, so store files to specify if the process has finished
function isDominated(){
	f="$1"
	if [ ! -f "$fastest_dir/${f:5}.param.fastest" ]; then
		echo 0
	elif [ ! -f $results_dir/${f}.zfinished ]; then
		# Not allways 1
		# only Dominated if  the timeout is  >  DOMINATION_MULTIPLIER  * fastest
		(( fastest =  `head -n 1 $fastest_dir/${f:5}.param.fastest` * ${DOMINATION_MULTIPLIER:-2}))
		(( dominated =  TOTAL_TIMEOUT > fastest  ))
		echo $dominated
	else
		(( fastest =  `head -n 1 $fastest_dir/${f:5}.param.fastest` * ${DOMINATION_MULTIPLIER:-2}))
		# TODO use SR + Minion time
		(( dominated  = `grep "real" $results_dir/$f.time.all | tail -n1 | sed -Ee 's/.*m([0-9]+).*/\1/'` > $fastest ))
		echo $dominated
	fi
}

echo "<$0> using *.param"

export -f isDominated
export  results_dir
export  fastest_dir
export TOTAL_TIMEOUT

parallel   --tagstring "{/}"  'echo "isDominated:$(isDominated {/.})"'  \
	::: `ls ${results_dir}/*${param_glob}.zstarted` \
	|   runhaskell ${Script_Base}/db/gather_data.hs  ${Essence_base} \
	|   sqlite3 ${REPOSITORY_BASE}/results.db

function addParamIndexes(){
	f="$1"
	sed '1,/\$SQL\$/d' "$results_dir/../params/${f:5}.param" \
		| cut -c 2- \
		| sqlite3 ${REPOSITORY_BASE}/results.db
}

export -f addParamIndexes

parallel  -j${NUM_JOBS:-6} --tagstring "{/}"  'addParamIndexes "{/.}"'  \
	::: `ls ${results_dir}/*${param_glob}.zstarted`

# So we know which minion were created
ls ${results_dir}/*${param_glob}.minion  >> "${stats_dir}/_${Essence_base}.minions"
echo ""  >> "${stats_dir}/_${Essence_base}.minions"

echo "<$0> finished"