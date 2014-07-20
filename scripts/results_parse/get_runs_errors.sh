#!/bin/bash
set -o nounset


find ${1} -name state.json | parallel -j${NUM_JOBS:-4} "$PARAM_GEN_SCRIPTS/results_parse/get_runs_errors.py {} summary"