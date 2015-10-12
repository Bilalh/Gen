.mode column
.width 9 7 8 11 11 12 7 7 7
.headers on
Select paramHash, quality, ordering,
	eprimesLeft, Satisfiable, MaxSolutions,
	minTime, maxTime, avgTime
From ParamsData
Limit 10
;
