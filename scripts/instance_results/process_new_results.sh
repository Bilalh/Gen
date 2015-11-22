#!/bin/bash
# Process the new results, in parallel
set -o nounset
OUR="$( cd "$( dirname "$0" )" && pwd )";
set -x
"${OUR}/fix_ParamsData.sh"     \
  && "${OUR}/mk_summary.sh"    \
  && "${OUR}/get_summary.sh"   \
  && "${OUR}/add_groups.py"    \
  && "${OUR}/make_excluded.sh" \
  && ( [ -f r/all_models.csv.bin ] && rm r/all_models.csv.bin ) || true
set +x