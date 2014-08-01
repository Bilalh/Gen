#!/bin/bash
# Bilal Syed Hussain


# print the command the run it
function echoing(){
	echo "$@"
	"$@"
}

if [ -n "$ZSH_VERSION" ]; then
	exportf () {
		export $(echo $1)="`whence -f $1 | sed -e "s/$1 //" `"
	}
else
    exportf (){
        export -f $1
    }
fi

# Runs the whole toolchain using compact with specifed params
function csolve(){
	if [ $# -lt 1 ]; then
		echo "$0  param+ "
	else
		local base=`basename $PWD`
		__models_dir="${base}-compact/"

		if ( cr ); then
			pushd ${base}-compact >/dev/null
			export LINES_TO_SHOW=${LINES_TO_SHOW:-20}
			refine_run ${EXTRA:-} ../${base}.essence ::: 0001.eprime ::: "$@"
			popd > /dev/null
		fi
	fi
}


# refine params then solve them on all eprimes
function refine_run(){
	if [ $# -lt 3 ]; then
		echo "$0 [all] essence ::: eprime+ ::: param+ "
	else
		local __all_solution=0
		if [ "$1" = "all" ]; then
			__all_solution=1
			shift
		fi
		if [ "$1" = "c" ]; then
			__all_solution=2
			shift
		fi
		if [ "$1" = "d" ]; then
			__all_solution=3
			shift
		fi
		if [ "$1" = "rnd"  ]; then
			export MINION_OPTIONS="-solver-options -randomiseorder"
			shift
		fi
		export __all_solution

		local essence=$1
		shift

		export COLUMNS
		exportf __refine_par_func
		exportf refine_param
		exportf solve_eprime_param
		exportf up_eprime
		exportf vaildate_solution
		exportf minion_count_solutions
		exportf echoing
		parallel --keep-order --tagstring  "{2/.} {1/.}" -j${NUM_JOBS:-4} \
			"__refine_par_func {2} {1} ${essence}" "$@"
	fi
}



function cr(){
	local base=`basename $PWD`
	mkdir -p  ${base}-compact;
	echoing conjure \
	--mode compact \
	--out-eprime ${base}-compact/0001.eprime \
	--in-essence ${base}.essence +RTS -M16G
}


function cat_solutions(){

	local file_ext=${file_ext:-solution}

	local tagstring="{#}"
	if [ "$1" = "n" ]; then
		tagstring=""
		shift
	fi

	local extra=""
	local end="cat"
	if [ "$1" = "o" ]; then
		tagstring=""
		shift
		extra='| tr -d "\n"; echo'
	fi

	if [ "$1" = "s" ]; then
		tagstring=""
		shift
		extra=" | tr -d \"\n\" | sed -e 's/letting//g' | sed -E 's/  */ /g'"
		end=sort
	fi


	parallel --tagstring "${tagstring}" --keep-order -j${NUM_JOBS:-4}  "cat {} | sed '1d' ${line_func:-} ${extra}  " ::: ${1:-}*.${file_ext}* | ${end}
 	echo
	echo "Total: $(ls ${1:-}*.${file_ext}* | wc -l )  ${file_ext}"
}


function __refine_par_func(){
	local param=$1
	local eprime=$2
	local essence=$3

	local eprime_base=`basename ${eprime}`
	local param_base=`basename ${param}`
	local out_eparam="${eprime_base%.*}-${param_base%.*}.eprime-param"
	local eprime_solution="${eprime_base%.*}-${param_base%.*}.eprime-solution"

	if [  $__all_solution -ge 2 ]; then
		refine_param "$@" && \
		[ -f ${out_eparam} ] && \
		minion_count_solutions ${eprime} ${out_eparam}
	else
		refine_param "$@" && \
		[ -f ${out_eparam} ] && \
		solve_eprime_param ${eprime} ${out_eparam} && \
		[ -f $( ls ${eprime_solution}* | head -n 1 ) ]
		up_eprime ${eprime_solution}  ${param} ${out_eparam} ${essence} ${eprime}
	fi
}


function refine_param(){
    local param=$1
	local eprime=$2
	local essence=$3

	local param_base=`basename ${param}`
	local eprime_base=`basename ${eprime}`
	local out_param="${eprime_base%.*}-${param_base%.*}.eprime-param"

	echoing conjure --mode refineParam --in-eprime ${eprime} --in-essence-param ${param} --in-essence ${essence} --out-eprime-param ${out_param}

}

function solve_eprime_param(){
	local eprime=$1
	local eparam=$2

	local param_base=`basename ${eparam}`
	local out_minion="${param_base%.*}.minion"
	local out_solution="${param_base%.*}.eprime-solution"

	rm ${out_solution}*

	extra=""
	if [ ${__all_solution} -ge 1 ]; then
		extra='-all-solutions'
	fi

	echoing savilerow ${MINION_OPTIONS:--solver-options '-sollimit 10'} -in-eprime ${eprime} -in-param ${eparam} -run-solver  -out-minion ${out_minion} -out-solution ${out_solution} ${extra}

}


function minion_count_solutions(){
	local eprime=$1
	local eparam=$2

	local param_base=`basename ${eparam}`
	local out_minion="${param_base%.*}.minion"

	filter='Nodes|solvable|Solutions|Total|RSS'
	if [ $__all_solution -eq 3 ]; then
		filter='Solutions'
	fi
	echoing savilerow -in-eprime ${eprime} -in-param ${eparam} -out-minion ${out_minion} -all-solutions -run-solver
	echoing minion ${out_minion} -noprintsols -findallsols -preprocess SACBounds \
		| grep -v '#' | egrep $filter
}


function up_eprime(){
	local eprime_solution=$1
	local param=$2
	local eparam=$3
	local essence=$4
	local eprime=$5

	local eprime_solution_base=`basename ${eprime_solution}`
	local solution="${eprime_solution_base%.*}.solution"

	rm -f "${solution}"* > /dev/null

	if [ ${__all_solution} -ge 1 ]; then
		cmd="
		sol=\$( echo '{}' | sed -e 's/eprime-solution/solution/g'  );
		conjure
			--mode translateSolution
			--in-eprime ${eprime}
			--in-eprime-param ${param}
			--in-essence ${essence}
			--in-eprime-solution {} \
			--in-essence-param ${eparam}
			--out-solution \${sol} \
		&& head -n ${LINES_TO_SHOW:-10} \${sol} | sed '1d' | fmt -w$((COLUMNS - ${#pbase} - ${#ebase} - 5 )) \
            && [ -z "${NO_VAILDATE:-}" ] && vaildate_solution \${sol} ${param} ${essence} && echo Vaild"
		echo $cmd
		parallel --tagstring "{#}" --keep-order -j${NUM_JOBS_ALL_SOLS:-1} $cmd ::: ${eprime_solution}.*

		echo "Total: $(ls ${eprime_solution}.* | wc -l )  solution"


	else
		pbase="`basename param`"
		ebase="`basename eprime`"
		echoing conjure \
			--mode translateSolution \
			--in-eprime ${eprime} \
			--in-eprime-param ${eparam} \
			--in-essence ${essence} \
			--in-eprime-solution ${eprime_solution} \
			--in-essence-param ${param} \
			--out-solution ${solution} \
		&& head -n ${LINES_TO_SHOW:-10} ${solution} | sed '1d' | fmt -w$((COLUMNS - ${#pbase} - ${#ebase} - 5 )) \
		&& [ -z "${NO_VAILDATE:-}" ] && vaildate_solution ${solution} ${param} ${essence} && echo Vaild
	fi

}

function vaildate_solution(){
	local solution=$1
	local param=$2
	local essence=$3

	echoing conjure --mode validateSolution \
		--in-essence ${essence}  \
		--in-param ${param} \
		--in-solution ${solution}
}

function record_tools_versions(){
	local __commands_needed=(ghc python python3 pip sqlite3 git hg perl parallel pigz ruby)
	local __our_progs=(minion conjure savilerow essenceGivensToFinds essenceGivensToJson2 essenceLettingsToJson testGen essenceSolver)
	
	for prog in "$__commands_needed"; do
		if ! hash "$prog" 2>/dev/null; then
			printf "\e[01;31m Need $prog to be in the PATH \e[0m\n"	 >&2
			return 1
		fi
}
