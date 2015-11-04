#!/bin/bash
set -o nounset

# shellcheck disable=SC2016
parallel --keep-order  --line-buffer \
	--rpl '{fmt} $Global::use{"File::Basename"} ||= eval "use File::Basename; 1;"; $_ = dirname($_);$_=sprintf("%-80s",$_)' \
	--tagstring '{fmt}' \
	'cat {//}/summary/hittingSet; echo "" ' \
	:::: <(find . -type d -name 'fastest*') > hittingSet

# shellcheck disable=SC2016
parallel --keep-order  --line-buffer \
	--rpl '{fmt} $Global::use{"File::Basename"} ||= eval "use File::Basename; 1;"; $_ = dirname($_);$_=sprintf("%-80s",$_)' \
	--tagstring '{fmt}' \
	'cat {//}/summary/resultSet; echo "" ' \
	:::: <(find . -type d -name 'fastest*') > resultSet
