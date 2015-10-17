#!/bin/bash
set -o nounset
OUR="$( cd "$( dirname "$0" )" && pwd )";
export NUM_JOBS=1

results_dir="${OUT_BASE_DIR}/results_${USE_MODE}/"
#"
stats_dir="${OUT_BASE_DIR}/stats_${USE_MODE}/"
#"
fastest_dir="${OUT_BASE_DIR}/fastest_${USE_MODE}/"
#"

timing_method=${TIMING_METHOD:-cpu}
echo "timing_method: ${timing_method}"
export  timing_method


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

"${OUR}/init_db.sh"

param_glob=${PARAM_BASE_NAME}
echo "param_glob ${param_glob}"

param_num="`cat ${stats_dir}/${USE_DATE}.params-used | sort | uniq | wc -l | egrep -o '^ *[0-9]+' | tr -d ' '`"
eprime_num="`wc -l  $(ls ${stats_dir}/${USE_DATE}.models-used | tail -n1)  | egrep -o '^ *[0-9]+ ' | tr -d ' '`"

echo "param_num: $param_num"
echo "eprime_num $eprime_num"

while read minion_timeout total_timeout; do
	echo "INSERT OR REPLACE into Timeouts('paramHash', 'MinionTimeout','TotalTimeout', 'timestamp')
				  Values('${PARAM_BASE_NAME}', ${minion_timeout}, ${total_timeout}, '$USE_DATE' );" \
	|  sqlite3 ${REPOSITORY_BASE}/results.db
done < `ls ${stats_dir}/${USE_DATE}.timeout-used | tail -n1`


if (grep -c minimising "$Essence"); then
	minimising="1"
elif (grep -c maximising "$Essence"); then
	minimising="0"
else
	minimising="NULL"
fi
export minimising

echo "INSERT OR REPLACE into Metadata('essence', 'mode', 'minimising')
			  Values('${Essence_base}', '${USE_MODE}', '${minimising}');" \
	|  sqlite3 ${REPOSITORY_BASE}/results.db


echo "<$0> using *.minion-table"


function doMinionTable(){
	set -x
	fp="$1"
	sr_time="${results_dir}/$2.sr-time"


	"${OUR}/parse_minion_tableout_sql.py" "$fp" ${REPOSITORY_BASE}/results.db `grep ${timing_method} ${sr_time} | egrep -o '[0-9]+(.[0-9]+)?'`
	set +x
}

export results_dir
export OUR
export REPOSITORY_BASE
export -f doMinionTable

if [ "$( parallel --version | egrep -o '[0-9]+$'  )" -ge "20141122"  ]; then
	ls ${results_dir}/*${param_glob}*.minion-table | parallel -j1 \
		--rpl '{base} s:.*/::; s:\.[^/.]+$::; s:^model_::; $_=substr($_,0,-123) . "..";' \
		--tagstring "{#} {base}"  'echo $(doMinionTable {} {/.})'
else
	echo "INSTALL a newer version of parallel (>=20141122) for much nicer output "
	echo "INSTALL a newer version of parallel (>=20141122) for much nicer output " >2
	ls ${results_dir}/*${param_glob}*.minion-table | parallel -j1 --tagstring "{/.}"  'echo $(doMinionTable {} {/.})'
fi


function solutionValue(){
	set -x
	minion_out="$1.minion-output"

	if [ ! -f "${minion_out}" ]; then
		if [  "${minimising}" = "1" ]; then
			echo "2147483647"
		elif [  "${minimising}" = "0" ]; then
			echo "–2147483648"
		else
			echo "0"
		fi
	else
		if (grep -c  'Solution found with Value:' "${minion_out}" &>/dev/null ); then
			grep 'Solution found with Value:'  "${minion_out}" | \
			egrep -o '[0-9]+' | \
			ruby -e 'p $stdin.readlines.map(&:to_i).max'
		else
			if [  "${minimising}" = "1" ]; then
				echo "2147483647"
			elif [  "${minimising}" = "0" ]; then
				echo "–2147483648"
			else
				echo "0"
			fi
		fi
	fi
	set +x
}
export -f solutionValue

parallel -j"${NUM_JOBS}" --tagstring "{/}"  'echo "solutionValue:$(solutionValue {.})"'  \
	::: $(ls ${results_dir}/*${param_glob}.zstarted) 2>"${stats_dir}/${USE_DATE}.solutionValue-trace" \
	|   runhaskell "${OUR}/gather_data.hs"  ${Essence_base} \
	|   sqlite3 ${REPOSITORY_BASE}/results.db


set -x
solutionValue="$(sqlite3 ${REPOSITORY_BASE}/results.db "Select solutionValue from ParamSolutionValues where paramHash='${PARAM_BASE_NAME}';")"
echo "@@solutionValue:${solutionValue}"
set +x

# We store files .zfinished to specify if the process has finished
function isDominated(){
	set -x
	f="$1"
	sr_time="${results_dir}/$1.sr-time"
	minion_time="${results_dir}/$1.minion-time"

    # Now assuming we use smart file names
	# so we split model_1_1_1_1-5b0..4d9
	# into model_1_1_1_1 5b0..4d9
	# and use indexing to get 5b0..4d9
	f_arr=(${f//-/ })
	f_base="${f_arr[*]:1}"

	MIN_TOTAL_TIME=${MIN_TOTAL_TIME:-1}
	if [ ! -f "$fastest_dir/${f_base}.param.fastest" ]; then
		echo 0
	elif [ ! -f $results_dir/${f}.zfinished ]; then
		# Not allways 1
		# only Dominated if  the timeout is  >  DOMINATION_MULTIPLIER  * fastest
		(( fastest =  `head -n 1 $fastest_dir/${f_base}.param.fastest` * ${DOMINATION_MULTIPLIER:-2}))
		(( dominated  =  TOTAL_TIMEOUT > MIN_TOTAL_TIME &&  TOTAL_TIMEOUT > fastest ))
		echo $dominated
	else
		(( fastest =  `head -n 1 $fastest_dir/${f_base}.param.fastest` * ${DOMINATION_MULTIPLIER:-2}))
		(( time_taken  = `grep "${timing_method}" ${sr_time}     |  ruby -e 'print gets[4..-1].to_f.floor' ` ))
		(( time_taken  += `grep "${timing_method}" ${minion_time} |  ruby -e 'print gets[4..-1].to_f.floor' ` ))

		# floor was used since ceil could make a non-dominated model dominated
		(( dominated  =  time_taken > MIN_TOTAL_TIME &&  time_taken > $fastest ))
		echo $dominated
	fi
	set +x
}

echo "<$0> using *.param"

export -f isDominated
export  results_dir
export  fastest_dir
export TOTAL_TIMEOUT

parallel -j"${NUM_JOBS}" --tagstring "{/}"  'echo "isDominated:$(isDominated {/.})"'  \
	::: $(ls ${results_dir}/*${param_glob}.zstarted)  2>"${stats_dir}/${USE_DATE}.isDominated-trace" \
	|   runhaskell "${OUR}/gather_data.hs"  ${Essence_base} \
	|   sqlite3 ${REPOSITORY_BASE}/results.db


echo "$0 Calculate total solving time"
if (ls ${results_dir}/*${param_glob}.param-time &>/dev/null); then
time parallel -j"${NUM_JOBS}" "grep ${timing_method} {} | egrep -o '[0-9].*'  " ::: `ls ${results_dir}/*${param_glob}.param-time`  `ls ${results_dir}/*${param_glob}.sr-time` `ls ${results_dir}/*${param_glob}.minion-time` \
	| ruby -e 'p $stdin.readlines.map(&:to_f).reduce(&:+)' > ${stats_dir}/${USE_DATE}.total_solving_time
else
	# i.e  param refinement timed out
	echo 0 > ${stats_dir}/${USE_DATE}.total_solving_time
fi


# So we know which minion were created
echo "$0 Record minions files"
ls ${results_dir}/*${param_glob}.minion  >> "${stats_dir}/_${Essence_base}.minions"
echo ""  >> "${stats_dir}/_${Essence_base}.minions"

echo "<$0> finished"
