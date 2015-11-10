#!/bin/bash
set -o nounset
cores=${CORES:-"$(parallel --number-of-cores)"};

parallel -j"${cores}" --line-buffer --tagstring '{/.}' "gen instance-summary -o {//} -m sample-64" :::: <(find . -type d -name 'fastest*')
