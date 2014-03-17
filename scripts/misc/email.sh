#!/bin/bash

function record_experiment_info(){
	local _fp="$1"
	local fp="`python -c 'import os,sys; print(os.path.realpath(sys.argv[1]))'  ${_fp}`"
	echo "[Experiments](`hostname`) <`date +%F`>" > "${fp}"
	pushd /home/azureuser/repos/instancegen-models

	hostname >> "${fp}"

	du -sch results/ >> "${fp}"
	echo "" >> "${fp}"

	tree -L 4 results -I '*eprime*' -sh >> "${fp}"
	echo "" >> "${fp}"

	df -h >> "${fp}"
	echo "" >> "${fp}"

	if ( ls results/setting*.json ); then
		cat $(ls results/setting*.json | tail -n1) >> "${fp}"
		echo "" >> "${fp}"
	fi

	popd

	pushd /home/azureuser/repos/instancegen
	git diff >> "${fp}"
	popd

	pwd >> "${fp}"
	if ( git rev-parse --is-inside-work-tree ); then
		echo "repo(git) version" >> "${fp}"
		git log -1 --format="%H" >> "${fp}"
		git describe --always    >> "${fp}"

		git diff >> "${fp}"
	fi


	if ( hg summary ); then
		hg log -r . --template "{latesttag}-{latesttagdistance}-{node|short}" >> "${fp}"
		hg summary >> "${fp}"

		hg diff >> "${fp}"
	fi

	echo "" >> "${fp}"
	echo "Bilal" >> "${fp}"
}


function notify_experiment_finished(){
	local subject="[Experiments](`hostname`) <`date +%F`>"
	local address="bh246@st-andrews.ac.uk"
	local fp=$(mktemp /tmp/email-file.XXXXXXXX)
	record_experiment_info "${fp}"
	mail -s "${subject}" ${address} < "${fp}"
	echo "email sent to ${address}"
	rm "${fp}"
}

function record_experiment_info2(){
	local _fp="$1"
	local fp="`python -c 'import os,sys; print(os.path.realpath(sys.argv[1]))'  ${_fp}`"
	echo "[Experiments](`hostname`) <`date +%F`>" > "${fp}"

	pwd >> "${fp}"
	tree -L 2 >> "${fp}"
	echo "" >> "${fp}"

	if ( git rev-parse --is-inside-work-tree ); then
		echo "repo(git) version" >> "${fp}"
		git log -1 --format="%H" >> "${fp}"
		git describe --always    >> "${fp}"

		git diff >> "${fp}"
	fi


	if ( hg summary ); then
		hg log -r . --template "{latesttag}-{latesttagdistance}-{node|short}" >> "${fp}"
		hg summary >> "${fp}"

		hg diff >> "${fp}"
	fi

	echo "" >> "${fp}"
	echo "Bilal" >> "${fp}"
}

function notify_experiment_finished2(){
	local subject="[Experiments](`hostname`) <`date +%F`>"
	local address="bh246@st-andrews.ac.uk"
	local fp=$(mktemp /tmp/email-file.XXXXXXXX)
	record_experiment_info2 "${fp}"
	mail -s "${subject}" ${address} < "${fp}"
	echo "email sent to ${address}"
	rm "${fp}"
}

