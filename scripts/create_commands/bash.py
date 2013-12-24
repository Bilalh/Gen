
record_funcs="""
function record(){
	if [[ -z "${1}" || -z "${2}" ]]; then
		echo "$0 name command"
	else
		local name="$1"
		shift
		if [ "`basename \"${name}\"`" != "${name}" ]; then
			mkdir -p "`dirname \"${name}\"`"
		fi
		local fp="${name}-`date +%F_%H-%M_%s`"
		echo "pwd:`pwd`"> "${fp}.cmds"
		echo "$@"  >> "${fp}.cmds"
		$@ 2>&1 | tee "${fp}.log"
	fi
}


function record_cp(){
	if [[ -z "${1}" || -z "${2}" ]]; then
		echo "$0 name command"
	else
		local name="$1"
		shift
		if [ "`basename \"${name}\"`" != "${name}" ]; then
			mkdir -p "`dirname \"${name}\"`"
		fi
		local fp="${name}-`date +%F_%H-%M_%s`"
		echo "pwd:`pwd`"> "${fp}.cmds"

		echo "conjure">> "${fp}.cmds"
		conjure 2>&1 | grep Version						  >> "${fp}.cmds"
		minion 2>&1	 | egrep 'Git version|Minion Version' >> "${fp}.cmds"
		savilerow	 | head -n 2 | tail -n 1			  >> "${fp}.cmds"

		local sr="$(dirname `which savilerow`)"
		if [ -f "${sr}/.hg/cache/branchheads" ]; then
			cat "${sr}/.hg/cache/branchheads" >> "${fp}.cmds"
		fi
		echo "Command:"  >> "${fp}.cmds"
		echo "$@"        >> "${fp}.cmds"
		$@ 2>&1 | tee "${fp}.log"
	fi
}

# only "True" is true
function to_bool(){
	if [ "$1" == "True" ]; then
		echo 1
	else
		echo 0
	fi
}

export -f record
export -f record_cp
export -f to_bool
"""