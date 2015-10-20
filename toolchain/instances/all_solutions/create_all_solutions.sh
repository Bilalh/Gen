#!/bin/bash
# Create all solutions for use with gen-instance*

OUR_DIR="$( cd "$( dirname "$0" )" && pwd )";
NUM_JOBS="${CORES:-"$(parallel --number-of-cores)"}"
export NUM_JOBS

"$(gen --toolchain-path)/instances/all_solutions/pre_create_all_params_from_essence_par_no_up.sh" \
  86400 essence_param_find.essence essence_param_find.eprime "$OUR_DIR"
