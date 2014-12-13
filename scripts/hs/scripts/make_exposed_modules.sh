#!/bin/bash
set -o nounset
find -L src -name '*.hs' | egrep -v 'Example|Old|exec|EssenceSolver|Development' | sed -e 's!src/!!' -e 's@.hs@@' -e 's/^/, /' -e 's@/@.@g' | pbcopy; pbpaste
