list.of.packages <- c("plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(plyr)

base <- path.expand("~/Desktop/Results/sampling_no_large/")
all <- read.csv(file.path(base, "all.csv"))

info=ddply(all, c("essenceClass", "kind", "heuristic", "mode","group", "refineGroup" ), summarise,
           runs=length(run_no),
           highestOrderingMean=mean(highestOrderingNeeded),
           highestOrderingNeeded=toString(highestOrderingNeeded),
           compactWon=toString(compactWon),
           numFractures=toString(numFractures),
           fracturesSize=toString(fracturesSize),
           # rIterationsDone=mean(rIterationsDone),
           rIterationsDoneIncludingFailed=toString(rIterationsDoneIncludingFailed),
           hostType=toString(hostType),
           rCPUTime=mean(rCPUTime),
           rRealTime=mean(rRealTime),
           fCPUTime=mean(rCPUTime)/3600,
           fRealTime=mean(rRealTime)/3600
           )


finished=info[info$runs==3,]
write.csv(finished, file="summary.csv")

# View(finished)
# View(all)
names.essence <- unique(all$essenceClass)
selected.ppp  <- all[ all$essenceClass=="prob013-PPP"       &  all$kindClass=="undirected" & all$mode=="sample-64" , ]
selected.wh   <- all[ all$essenceClass=="prob034-warehouse" &  all$kindClass=="undirected" & all$mode=="sample-64" , ]
selected.bacp <- all[ all$essenceClass=="prob030-BACP"      &  all$kindClass=="undirected" & all$mode=="sample-64" , ]
selected.efpa <- all[ all$essenceClass=="prob055-efpa"      &  all$kindClass=="undirected" & all$mode=="sample-64" , ]
selected.sgp <- all[ all$essenceClass=="prob010-SGP"        &  all$kindClass=="undirected" , ]

# View(selected)

