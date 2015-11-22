list.of.packages <- c("plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(plyr)

base <- path.expand("~/Desktop/Results/sampling_no_large/")
all <- read.csv(file.path(base, "all.csv"))

info=ddply(all, c("essenceClass", "kind", "heuristic", "mode","group"), summarise,
           runs=length(run_no),
           highestOrderingMean=mean(highestOrderingNeeded),
           highestOrderingNeeded=toString(highestOrderingNeeded),
           compactWon=toString(compactWon),
           numFractures=toString(numFractures),
           # rIterationsDone=mean(rIterationsDone),
           rIterationsDoneIncludingFailed=toString(rIterationsDoneIncludingFailed),
           hostType=toString(hostType),
           rCPUTime=mean(rCPUTime),
           rRealTime=mean(rRealTime),
           fCPUTime=mean(rCPUTime)/3600,
           fRealTime=mean(rRealTime)/3600
           )


finished=info[info$runs==3,]
# View(finished)
write.csv(finished, file="summary.csv")

View(all)
