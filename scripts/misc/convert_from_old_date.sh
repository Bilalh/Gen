#!/bin/bash
set -o nounset
set -o errexit

vd="$1"

if (sw_vers &>/dev/null); then
	version_date="$(date -jf '%Y-%m-%e %H:%M %z' "${vd}" '+%F_%s')"
else
	version_date="$(date --date="${vd}" '+%F_%s')"
fi

mv "$vd" "$version_date"

