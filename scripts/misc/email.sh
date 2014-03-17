#!/bin/bash

function record_experiment_info(){
	local _fp="$1"
	local fp="`python -c 'import os,sys; print(os.path.realpath(sys.argv[1]))'  ${_fp}`"
	echo "" > $fp
	pushd /home/azureuser/repos/instancegen-models

	hostname >> $fp

	du -sch results/ >> $fp
	echo "" >> $fp

	tree -L 4 results -I '*eprime*' -sh >> $fp
	echo "" >> $fp

	df -h >> $fp
	echo "" >> $fp

	if ( ls results/setting*.json ); then
		cat `ls results/setting*.json | tail -n1` >> $fp 
		echo "" >> $fp
	fi

	popd

	pushd /home/azureuser/repos/instancegen
	git diff >> $fp
	popd

	echo "" >> $fp
	echo "Bilal" >> $fp
}


function notify_experiment_finished(){
	local subject="[Experiments](`hostname`) Finished"
	local address="bh246@st-andrews.ac.uk"
	local fp=$(mktemp /tmp/email-file.XXXXXXXX)
	record_experiment_info ${fp}
	mail -s "${subject}" ${address} < $fp
	echo "email sent to ${address}"
	rm ${fp}
}
