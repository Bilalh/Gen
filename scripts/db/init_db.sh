#!/bin/bash
set -o nounset

# get the repository base
DIR="$( cd "$( dirname "$0" )" && pwd )"
DIR="$DIR/../../"

sqlite3 "${REPOSITORY_BASE}/results.db" <<SQL
	CREATE TABLE IF NOT EXISTS  "Experiment" (
	"eprime" TEXT NOT NULL,
	"paramHash" TEXT NOT NULL,
	"attribute" TEXT NOT NULL,
	"value" REAL,
	PRIMARY KEY ("eprime", "paramHash", "attribute")
	);


	CREATE TABLE IF NOT EXISTS "ParamQuality" (
	"param" TEXT NOT NULL UNIQUE,
	"quality" REAL NOT NULL,
	"ordering" INTEGER PRIMARY KEY NOT NULL UNIQUE,
	"paramHash" TEXT NOT NULL UNIQUE
	);

	CREATE TABLE IF NOT EXISTS "Metadata" (
	"essence" TEXT PRIMARY KEY NOT NULL UNIQUE
	);

	CREATE TABLE IF NOT EXISTS "Timeouts" (
	"paramHash" TEXT NOT NULL PRIMARY KEY NOT NULL UNIQUE,
	"MinionTimeout" REAL NOT NULL,
	"TotalTimeout" REAL NOT NULL,
	"timestamp" TEXT NOT NULL
	);



	CREATE VIEW IF NOT EXISTS Attributes as
	Select DISTINCT attribute
	From Experiment
	Order by attribute;


	CREATE VIEW IF NOT EXISTS TimingsDomination as

	Select *

	From(
		Select SR.paramHash, SR.eprime, SavileRow, Minion,  (SavileRow + Minion) as TotalTime,
	           Cast(MinionNodes AS Integer) as MinionNodes, MinionTimeout,
			   MinionSatisfiable, MinionSolutionsFound,
			   (MinionSatisfiable = 1 and MinionTimeOut = 0) as IsOptimum, isDominated
		From (
			Select eprime, paramHash, f.value as SavileRow From Experiment f
			Where f.attribute='SavileRowTotalTime'
			Order By paramHash, eprime
		) SR Join (
			Select eprime, paramHash, f.value as Minion From Experiment f
			Where f.attribute='MinionTotalTime'
			Order By paramHash, eprime
		) M Join (
			Select eprime, paramHash, Cast(f.value as Integer) as MinionNodes From Experiment f
			Where f.attribute='MinionNodes'
			Order By paramHash, eprime
		) N Join (
			Select eprime, paramHash, Cast(f.value as Integer) as MinionSatisfiable From Experiment f
			Where f.attribute='MinionSatisfiable'
			Order By paramHash, eprime
		) MS Join (
			Select eprime, paramHash, Cast(f.value as Integer) as MinionSolutionsFound From Experiment f
			Where f.attribute='MinionSolutionsFound'
			Order By paramHash, eprime
		) MF Join (
			Select eprime, paramHash, Cast(f.value as Integer) as MinionTimeOut From Experiment f
			Where f.attribute='MinionTimeOut'
			Order By paramHash, eprime
		) MT  Join (
			Select eprime, paramHash, Cast(f.value as Integer) as isDominated From Experiment f
			Where f.attribute='isDominated'
			Order By paramHash, eprime
		) DO

		on  SR.eprime = M.eprime And M.eprime  = N.eprime And N.eprime  = MS.eprime And MS.eprime  = MF.eprime And MF.eprime  = MT.eprime And MT.eprime  = DO.eprime
		and SR.paramHash  = M.paramHash  And M.paramHash   = N.paramHash  And N.paramHash   = MS.paramHash  And MS.paramHash   = MF.paramHash  And MF.paramHash   = MT.paramHash  And MT.paramHash   = DO.paramHash

		Union

		-- Unioning the eprime that never finished
		Select paramHash , eprime,
		  -1 as SavileRow, -1 as Minion, -1 as TotalTime,
		  -1 as MinionNodes ,1 as MinionTimeout,
		   0 as MinionSatisfiable , 0 as MinionSolutionsFound,
		   0 as IsOptimum, Cast(f.value as Integer) as isDominated
		From  Experiment f
		Where f.attribute = 'isDominated' and (
			Select count(attribute) From Experiment g
			Where f.eprime = g.eprime and f.paramHash = g.paramHash
		) = 1

	)

	Order by paramHash, eprime
		;



	CREATE VIEW IF NOT EXISTS TimingsRecorded as

	Select SR.paramHash, SR.eprime, SavileRow, Minion,  (SavileRow + Minion) as TotalTime,
           Cast(MinionNodes AS Integer) as MinionNodes, MinionTimeout,
		   MinionSatisfiable, MinionSolutionsFound,
		   (MinionSatisfiable = 1 and MinionTimeOut = 0) as IsOptimum, isDominated
	From (
		Select eprime, paramHash, f.value as SavileRow From Experiment f
		Where f.attribute='SavileRowTotalTime'
		Order By paramHash, eprime
	) SR Join (
		Select eprime, paramHash, f.value as Minion From Experiment f
		Where f.attribute='MinionTotalTime'
		Order By paramHash, eprime
	) M Join (
		Select eprime, paramHash, Cast(f.value as Integer) as MinionNodes From Experiment f
		Where f.attribute='MinionNodes'
		Order By paramHash, eprime
	) N Join (
		Select eprime, paramHash, Cast(f.value as Integer) as MinionSatisfiable From Experiment f
		Where f.attribute='MinionSatisfiable'
		Order By paramHash, eprime
	) MS Join (
		Select eprime, paramHash, Cast(f.value as Integer) as MinionSolutionsFound From Experiment f
		Where f.attribute='MinionSolutionsFound'
		Order By paramHash, eprime
	) MF Join (
		Select eprime, paramHash, Cast(f.value as Integer) as MinionTimeOut From Experiment f
		Where f.attribute='MinionTimeOut'
		Order By paramHash, eprime
	) MT  Join (
		Select eprime, paramHash, Cast(f.value as Integer) as isDominated From Experiment f
		Where f.attribute='isDominated'
		Order By paramHash, eprime
	) DO

	on  SR.eprime = M.eprime And M.eprime  = N.eprime And N.eprime  = MS.eprime And MS.eprime  = MF.eprime And MF.eprime  = MT.eprime And MT.eprime  = DO.eprime
	and SR.paramHash  = M.paramHash  And M.paramHash   = N.paramHash  And N.paramHash   = MS.paramHash  And MS.paramHash   = MF.paramHash  And MF.paramHash   = MT.paramHash  And MT.paramHash   = DO.paramHash

	Order by SR.paramHash, SR.eprime
		;


	CREATE VIEW IF NOT EXISTS DiscriminatingParams as
    Select P.paramHash, P.quality, P.ordering,
    	Cast(count(eprime) as Integer) as eprimes_count, Cast(max(D.MinionSatisfiable) as Integer) as Satisfiable, Cast(max(MinionSolutionsFound) as Integer) as MaxSolutions,
    	group_concat(D.eprime, ", ") as eprimes
    From ParamQuality P
    Join TimingsDomination D on P.paramHash = D.paramHash
    Where P.Quality < 1 and isDominated = 0
    Group by P.paramHash
    Order by P.quality
        ;

	CREATE VIEW IF NOT EXISTS ParamsData as
	    Select P.paramHash, P.param, Cast(T.TotalTimeout as Integer) as modelTimeout, T.timestamp ,  P.quality, P.ordering,
    	Cast(count(eprime) as Integer) as eprimes_count, Cast(max(D.MinionSatisfiable) as Integer) as Satisfiable, Cast(max(MinionSolutionsFound) as Integer) as MaxSolutions,
    	group_concat(D.eprime, ", ") as eprimes
    From ParamQuality P
    Join TimingsDomination D on P.paramHash = D.paramHash
	Join Timeouts T on T.paramHash = D.paramHash
    Where isDominated = 0
    Group by P.paramHash
    Order by P.quality
    	;


	CREATE VIEW IF NOT EXISTS Fastest as
	Select paramHash, eprime, min(TotalTime) as Time
	From TimingsRecorded
	Group by paramHash
		;

	CREATE VIEW IF NOT EXISTS FastestMinion as
	Select paramHash, eprime, min(Minion) as Time
	From TimingsRecorded
	Group by paramHash
		;

	CREATE VIEW IF NOT EXISTS FastestSavileRow as
	Select paramHash, eprime, min(SavileRow) as Time
	From TimingsRecorded
	Group by paramHash
		;




	CREATE VIEW IF NOT EXISTS EprimeOrdering as
	-- Eprimes which were the fastest on some paramHash
	Select eprime, 0 as Ord, count as Ord2,  avg as Ord3  From (

	Select eprime, count(*) as count, avg(Time)  as avg
	From Fastest
	Group by eprime
	)

	UNION

	-- Eprimes that did not timeout on some paramHash
	Select eprime, 1 as Ord, count as Ord2, avg as Ord3  From (
	Select eprime, count(*) as count, avg(TotalTime) as avg
	From TimingsRecorded
	Where eprime not in (Select Distinct eprime From Fastest)
	Group by eprime
	)

	UNION

	-- All other eprimes
	Select eprime, 2 as Ord, -1 as Ord2, -1 as Ord3 From(
		Select eprime
		From TimingsDomination
		Where eprime not in (Select Distinct eprime From TimingsRecorded)
	)

	Order by Ord asc, Ord2 desc, Ord3 asc
		;

SQL
