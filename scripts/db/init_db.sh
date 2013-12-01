#!/bin/bash
set -o nounset

# get the repository base
DIR="$( cd "$( dirname "$0" )" && pwd )"
DIR="$DIR/../../"

sqlite3 ${REPOSITORY_BASE}/results.db <<SQL
	CREATE TABLE IF NOT EXISTS  "Experiment" (
	"eprime" TEXT NOT NULL,
	"param" TEXT NOT NULL,
	"attribute" TEXT NOT NULL,
	"value" REAL,
	PRIMARY KEY ("eprime", "param", "attribute")
	);


	CREATE TABLE IF NOT EXISTS "ParamQuality" (
	"param" TEXT NOT NULL UNIQUE,
	"quality" REAL NOT NULL,
	"ordering" INTEGER PRIMARY KEY NOT NULL UNIQUE
	);

	CREATE TABLE IF NOT EXISTS "Models" (
	"Essence" TEXT PRIMARY KEY ASC NOT NULL UNIQUE,
	"Model #" INTEGER NOT NULL,
	"Param #" INTEGER NOT NULL,
	"MinionTimeout" REAL,
	"TotalTimeout" REAL
	);

	CREATE TABLE IF NOT EXISTS "ParamIndexes" (
	"param" TEXT NOT NULL,
	"paramPartNo" INTEGER NOT NULL,
	"paramIndex" INTEGER NOT NULL,
	PRIMARY KEY ("param", "paramPartNo", "paramIndex")
	);

	CREATE VIEW IF NOT EXISTS Attributes as
	Select DISTINCT attribute
	From Experiment
	Order by attribute;


	CREATE VIEW IF NOT EXISTS TimingsDomination as

	Select *

	From(
		Select SR.param, SR.eprime, SavileRow, Minion,  (SavileRow + Minion) as TotalTime,
	           Cast(MinionNodes AS Integer) as MinionNodes, MinionTimeout,
			   MinionSatisfiable, MinionSolutionsFound,
			   (MinionSatisfiable = 1 and MinionTimeOut = 0) as IsOptimum, isDominated
		From (
			Select eprime, param, f.value as SavileRow From Experiment f
			Where f.attribute='SavileRowTotalTime'
			Order By param, eprime
		) SR Join (
			Select eprime, param, f.value as Minion From Experiment f
			Where f.attribute='MinionTotalTime'
			Order By param, eprime
		) M Join (
			Select eprime, param, Cast(f.value as Integer) as MinionNodes From Experiment f
			Where f.attribute='MinionNodes'
			Order By param, eprime
		) N Join (
			Select eprime, param, Cast(f.value as Integer) as MinionSatisfiable From Experiment f
			Where f.attribute='MinionSatisfiable'
			Order By param, eprime
		) MS Join (
			Select eprime, param, Cast(f.value as Integer) as MinionSolutionsFound From Experiment f
			Where f.attribute='MinionSolutionsFound'
			Order By param, eprime
		) MF Join (
			Select eprime, param, Cast(f.value as Integer) as MinionTimeOut From Experiment f
			Where f.attribute='MinionTimeOut'
			Order By param, eprime
		) MT  Join (
			Select eprime, param, Cast(f.value as Integer) as isDominated From Experiment f
			Where f.attribute='isDominated'
			Order By param, eprime
		) DO

		on  SR.eprime = M.eprime And M.eprime  = N.eprime And N.eprime  = MS.eprime And MS.eprime  = MF.eprime And MF.eprime  = MT.eprime And MT.eprime  = DO.eprime
		and SR.param  = M.param  And M.param   = N.param  And N.param   = MS.param  And MS.param   = MF.param  And MF.param   = MT.param  And MT.param   = DO.param

		Union

		-- Unioning the eprime that never finished
		Select param , eprime,
		  -1 as SavileRow, -1 as Minion, -1 as TotalTime,
		  -1 as MinionNodes ,1 as MinionTimeout,
		   0 as MinionSatisfiable , 0 as MinionSolutionsFound,
		   0 as IsOptimum, Cast(f.value as Integer) as isDominated
		From  Experiment f
		Where f.attribute = 'isDominated' and (
			Select count(attribute) From Experiment g
			Where f.eprime = g.eprime and f.param = g.param
		) = 1

	)

	Order by param, eprime
		;



	CREATE VIEW IF NOT EXISTS TimingsRecorded as

	Select SR.param, SR.eprime, SavileRow, Minion,  (SavileRow + Minion) as TotalTime,
           Cast(MinionNodes AS Integer) as MinionNodes, MinionTimeout,
		   MinionSatisfiable, MinionSolutionsFound,
		   (MinionSatisfiable = 1 and MinionTimeOut = 0) as IsOptimum, isDominated
	From (
		Select eprime, param, f.value as SavileRow From Experiment f
		Where f.attribute='SavileRowTotalTime'
		Order By param, eprime
	) SR Join (
		Select eprime, param, f.value as Minion From Experiment f
		Where f.attribute='MinionTotalTime'
		Order By param, eprime
	) M Join (
		Select eprime, param, Cast(f.value as Integer) as MinionNodes From Experiment f
		Where f.attribute='MinionNodes'
		Order By param, eprime
	) N Join (
		Select eprime, param, Cast(f.value as Integer) as MinionSatisfiable From Experiment f
		Where f.attribute='MinionSatisfiable'
		Order By param, eprime
	) MS Join (
		Select eprime, param, Cast(f.value as Integer) as MinionSolutionsFound From Experiment f
		Where f.attribute='MinionSolutionsFound'
		Order By param, eprime
	) MF Join (
		Select eprime, param, Cast(f.value as Integer) as MinionTimeOut From Experiment f
		Where f.attribute='MinionTimeOut'
		Order By param, eprime
	) MT  Join (
		Select eprime, param, Cast(f.value as Integer) as isDominated From Experiment f
		Where f.attribute='isDominated'
		Order By param, eprime
	) DO

	on  SR.eprime = M.eprime And M.eprime  = N.eprime And N.eprime  = MS.eprime And MS.eprime  = MF.eprime And MF.eprime  = MT.eprime And MT.eprime  = DO.eprime
	and SR.param  = M.param  And M.param   = N.param  And N.param   = MS.param  And MS.param   = MF.param  And MF.param   = MT.param  And MT.param   = DO.param

	Order by SR.param, SR.eprime
		;


	CREATE VIEW IF NOT EXISTS DiscriminatingParams as
    Select P.*, group_concat(D.eprime, ", ") as eprimes
    From ParamQuality P
    Join TimingsDomination D on P.param = D.param
    Where P.Quality < 1 and isDominated = 0
    Group by P.param
    Order by P.quality
        ;


	CREATE VIEW IF NOT EXISTS Fastest as
	Select param, eprime, min(TotalTime) as Time
	From TimingsRecorded
	Group by param
		;

	CREATE VIEW IF NOT EXISTS FastestMinion as
	Select param, eprime, min(Minion) as Time
	From TimingsRecorded
	Group by param
		;

	CREATE VIEW IF NOT EXISTS FastestSavileRow as
	Select param, eprime, min(SavileRow) as Time
	From TimingsRecorded
	Group by param
		;



SQL
