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
info3 <- info2[ ! info2$group %in% c(16044, 14016, 12017, 12019, 12020, 12018, 16045), ]

info3.t <- info3[c("essenceClass", "kindClass", "heuristic", "num_models", "runs", "fracturesSize", "numFractures",  "highestOrderingNeeded", "highestOrderingMean", "compactWon", "pointsRejected")]
info3.th <- split( info3.t , info3.t$heuristic)


info3.h <- split( info3 , info3$heuristic)
info3.c <- split( info3 , info3$essenceClass)
info3.k <- split( info3 , info3$kind)


View(info3)
# View(info3.th$sdf)
