#!/bin/bash
set -o nounset

mkdir -p zmissing

parallel '[ -f {}/models.csv  ] || echo {}' :::: <(find . -name summary) > zmissing/models.txt
parallel '[  -f {}/summary.csv  ] || echo {}' :::: <(find . -name summary) > zmissing/summary.txt
