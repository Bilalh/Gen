Drop VIEW ParamsData;
CREATE VIEW ParamsData as
Select P.paramHash, P.param, Cast(T.TotalTimeout as  Integer) as modelTimeoutUsed, T.timestamp ,
 P.quality, P.ordering, Cast(count(D.eprime) as Integer) as eprimesLeft,
Cast(max(D.MinionSatisfiable) as Integer) as Satisfiable, Cast(max(D.MinionSolutionsFound) as Integer) as MaxSolutions,
F.minTime, F.maxTime, F.avgTime, paramCpuTime, numFinished,
TR.eprimesIds,
TR.eprimes

From ParamQuality P

Join TimingsDomination D on P.paramHash = D.paramHash
Join Timeouts T on T.paramHash = D.paramHash

Left Join (Select paramHash,
		min(TotalTime) as minTime, max(TotalTime) as maxTime, avg(TotalTime) as avgTime
	From TimingsRecorded Group by paramHash) F
  on F.paramHash = D.paramHash

Join ( Select paramHash,
	   count() as numFinished,
		group_concat(TR.eprimeId, ", ") as eprimesIds,
		group_concat(TR.eprime, ", ") as eprimes
	   From TimingsRecorded TR
	   Group By paramHash) TR
	 on TR.paramHash = D.paramHash

Where D.isDominated = 0 and D.MinionTimeOut = 0
Group by P.paramHash
Order by P.quality
	;

