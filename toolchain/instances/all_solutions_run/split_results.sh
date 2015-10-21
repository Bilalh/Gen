#!/bin/bash
# Create all solutions for use with gen-instance*

OUR_DIR="$( cd "$( dirname "$0" )" && pwd )";
NUM_JOBS="${CORES:-"$(parallel --number-of-cores)"}"
export NUM_JOBS

"$(gen --toolchain-path)/instances/all_solutions/split_solutions.sh" \
  "$OUR_DIR/all_sols"
