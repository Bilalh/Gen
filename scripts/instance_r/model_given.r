# Run models.r to generate the cached results
load("all_models.csv.bin")

list.of.packages <- c("plyr", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(plyr)
library(ggplot2)
library(scales)

prob = parts$prob034_warehouse

# We have run the params on multiple heuristics
mult <- prob[  ! is.na(prob$givenRunGroup)  , ]

win <- mult[ mult$isWinner ==1
           & mult$paramQuality < 1
           , ]
(win.names <- unique(win$eprime))

win$totalTimeMarked <- win$totalTime
win$totalTimeMarked[is.na(win$totalTimeMarked)] <- 600

win.times <- ddply(win, c("essenceClass", "kind", "heuristic", "mode", "eprime", "run_no", "givenRunGroup"), summarise,
                   avgRunTime_noNa=mean(totalTime, na.rm=TRUE),
                   avgRunTime_marked=mean(totalTimeMarked),
                   minRunTime=min(totalTime, na.rm = TRUE),
                   maxRunTime=max(totalTime, na.rm = TRUE),
                   avgNodes_noNa=mean(minionNodes, na.rm=TRUE)
)

