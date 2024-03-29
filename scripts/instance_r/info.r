list.of.packages <- c("plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(plyr)

base <- path.expand("~/Desktop/Results/sampling_no_large/")
all <- read.csv(file.path(base, "all.csv"))

all$pointsRejected <- all$rIterationsDoneIncludingFailed - all$rIterationsDone
all$pointsRejected[ all$kindClass != 'nsample' ] <- NA


server.babbage <- read.csv(file.path(base, "babbage",  "experiment.csv"))
server.eno <- read.csv(file.path(base, "eno",  "experiment.csv"))
server.ferry <- read.csv(file.path(base, "ferry",  "experiment.csv"))


info=ddply(all, c("essenceClass", "kindClass", "heuristic", "mode","num_models", "per_model_time_given", "group", "refineGroup" ), summarise,
           group2=toString(group2),
           runs=length(run_no),
           highestOrderingMean=mean(highestOrderingNeeded),
           highestOrderingNeeded=toString(highestOrderingNeeded),
           compactWon=toString(compactWon),
           numFractures=toString(numFractures),
           fracturesSize=toString(fracturesSize),
           rIterationsDoneIncludingFailed=toString(rIterationsDoneIncludingFailed),
           pointsRejected =toString(pointsRejected),
           hostType=toString(hostType),
           seq=toString(seq),
           kind =toString(kind),
           paramGroup=toString(paramGroup),
           tRealTime=sum(rRealTime),
           tCPUTime=sum(rCPUTime),
           rCPUTime=mean(rCPUTime),
           rRealTime=mean(rRealTime),
           fCPUTime=mean(rCPUTime)/3600,
           fRealTime=mean(rRealTime)/3600,
           influence_radius=toString(influence_radius),
           compact=toString(compact),
           fractures=toString(fractures),
           end=""
           )


names.essence <- unique(all$essenceClass)

info2 <- info[ ! info$refineGroup  %in%  c("2015-11-06_symlink", "2015-11-25") , ]

# Ignored because they are old versions, unused settings or unfinished problem classes, or run on differnt server

info.ignored <- c(16044, 14016, 12017, 12019, 12020, 12018, 16045, 14023, 14024, 14011,
                  12029, 12023,12015, 12028,14010)

info3 <- info2[ ! info2$group %in% info.ignored, ]
info3 <- info3[ ! info3$essenceClass %in% c("prob055-efpa", "prob028-BIBD", "prob038-steel^2", "prob038-steel^3",
                                            "prob006-GR", "prob010-SGP"), ]


info3.t <- info3[c("essenceClass", "kindClass", "heuristic", "num_models", "runs", "fracturesSize", "numFractures",  "highestOrderingNeeded", "highestOrderingMean", "compactWon", "pointsRejected")]
info3.th <- split( info3.t , info3.t$heuristic)


# info3.h <- split( info3 , info3$heuristic)
# info3.c <- split( info3 , info3$essenceClass)
# info3.k <- split( info3 , info3$kind)

times <- ddply( info3[info3$runs == 3 & info3$kindClass != "smac", ], c("essenceClass", "heuristic"), summarise,
                cpu  = as.integer(max(rCPUTime)),
                fcpu  = as.integer(max(rCPUTime)/3600),
                real  = as.integer(max(rRealTime)),
                freal = as.integer(max(rRealTime)/3600)
              )

times.with_smac <- ddply( info3[info3$runs == 3, ], c("essenceClass", "heuristic"), summarise,
                cpu  = as.integer(max(rCPUTime)),
                fcpu  = as.integer(max(rCPUTime)/3600),
                real  = as.integer(max(rRealTime)),
                freal = as.integer(max(rRealTime)/3600)
)

View(info3)
View(info3.t)
View(info3.th$sdf)
View(info3.th$wdeg)
View(info3.th$static)

info3.cpu.hours = sum(info3$tCPUTime) / 3600
info3.cpu.days  = info3.cpu.hours / 24
info3.cpu.years = info3.cpu.hours / 24 / 365

info3.real.hours = sum(info3$tRealTime) / 3600
info3.real.days = info3.real.hours / 24

