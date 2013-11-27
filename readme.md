

Query to get which model has the best (i.e lowest) quality for

Select P.*, group_concat(D.eprime, ", ") as eprimes
From ParamQuality P
Join TimingsDomination D on P.param = D.param
Where D.MinionTimeout = 0
Group by P.param