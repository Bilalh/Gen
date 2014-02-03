#!/bin/bash
# Bilal Syed Hussain
set -o nounset

set -x
essence="${1}"

mkdir -p "$OUT_BASE_DIR"
out_essence="${OUT_BASE_DIR}/essence_param_find.essence"
out_eprime="${OUT_BASE_DIR}/essence_param_find.eprime"

essenceGivensToFinds "${essence}" "${out_essence}" "`dirname ${essence}`/info.json"
conjure --mode compact  --in-essence "${out_essence}" --out-eprime "${out_eprime}"


echo "`date`"
set +x
