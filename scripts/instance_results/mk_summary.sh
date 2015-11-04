#!/bin/bash
set -o nounset
parallel -j1 --line-buffer --tagstring '{/.}' "gen instance-summary -o {//} -m sample-64" :::: <(find . -type d -name 'fastest*')
