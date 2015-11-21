#!/bin/bash
# Process the new results, in parallel 
set -o nounset
OUR="$( cd "$( dirname "$0" )" && pwd )";

"${OUR}/mk_summary.sh"       \
  && "${OUR}/get_summary.sh" \
  && "${OUR}/add_groups.py"  \
  && "${OUR}/make_excluded.sh"
