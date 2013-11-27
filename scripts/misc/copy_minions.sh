#!/bin/bash
# Bilal Syed Hussain

set -o nounset
cd /Volumes/Bs4Tb
rsync -av --compress /Users/bilalh/CS/paramgen minions --include '*/' --include '*.minion' --exclude '*'
find minions -empty -delete