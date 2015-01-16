#!/bin/bash
set -o nounset
ruby -ryaml -rjson -e 'data = YAML::load(STDIN.read); print data.to_json' <"$1"
