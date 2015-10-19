#!/bin/bash
set -o nounset
OUR="$( cd "$( dirname "$0" )" && pwd )";

sqlite3 "${REPOSITORY_BASE}/results.db" <<SQL
	DROP VIEW Attributes;
	DROP VIEW TimingsDomination;
	DROP VIEW TimingsRecorded;
	DROP VIEW DiscriminatingParams;
	DROP VIEW ParamsData;
	DROP VIEW Fastest;
	DROP VIEW FastestMinion;
	DROP VIEW FastestSavileRow;
	DROP VIEW EprimeOrdering;
	DROP VIEW ParamSolutionAllValues;
	DROP VIEW ParamSolutionValues;
SQL

"${OUR}/init_db.sh"