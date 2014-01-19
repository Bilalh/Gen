#!/bin/bash
# Bilal Syed Hussain

# print the command the run it
function echoing(){
    echo "$@"
    "$@"
}

function refine_param(){
	set -o nounset
    param=$1
	eprime=$2
	essence=$3
	
	param_base=`basename ${param}`
	eprime_base=`basename ${eprime}`
	out_param="${eprime_base%.*}-${param_base%.*}.eprime-param"
	
	echoing conjure --mode refineParam --in-eprime ${eprime} --in-essence-param ${param} --in-essence ${essence} --out-eprime-param ${out_param}
	
}

function run_eprime_param(){
	eprime=$1
	param=$2
	
	param_base=`basename ${param}`
	out_minion="${param_base%.*}.minion"
	out_solution="${param_base%.*}.eprime-solution"	
	
	echoing savilerow -in-eprime ${eprime} -in-param ${param} -run-minion minion -out-minion ${out_minion} -out-solution ${out_solution}

}

if [ $ZSH_VERSION ]; then
	
function _run_eprime_param(){
	_arguments \
		"1:eprimes:_files -g \*.eprime" \
		"2:params:_files -g \*.eprime-param"
}
	
compdef _run_eprime_param run_eprime_param

function _refine_param(){
	_arguments \
		"1:essence param:_files -g \*.param" \
		"2:eprime:_files -g \*.eprime" \
		"3:essence:_files -g \*.essence"
}

compdef _refine_param refine_param


fi