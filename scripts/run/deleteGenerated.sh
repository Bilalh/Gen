#!/bin/sh
# Bilal Syed Hussain
set -o nounset

dir=$1;
cd $1;

rm *.eprime-param *.eprime-solution *.minion *.minion-stats *.solution;
rm *.fails;
rm *.success;
rm *.time*;
rm *.output;
rm *.info;