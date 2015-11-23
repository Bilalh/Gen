# Run models.r to generate the cached results
load("all_models.csv.bin")

list.of.packages <- c("plyr", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(plyr)
library(ggplot2)
library(scales)

# prob <- parts$prob034_warehouse
prob <- models



# We have run the params on multiple heuristics
mult <- prob[  ! is.na(prob$givenRunGroup)  , ]

win <- mult[ mult$isWinner ==1
           & mult$paramQuality < 1
           , ]
(win.names <- unique(win$eprime))

win$totalTimeMarked <- win$totalTime
win$totalTimeMarked[is.na(win$totalTimeMarked)] <- 600

win.avgtimes <- ddply(win, c("essenceClass", "kind", "heuristic", "mode", "eprime", "run_no", "givenRunGroup"), summarise,
                   avgRunTime_noNa=mean(totalTime, na.rm=TRUE),
                   avgRunTime_marked=mean(totalTimeMarked),
                   minRunTime=min(totalTime, na.rm = TRUE),
                   maxRunTime=max(totalTime, na.rm = TRUE),
                   avgNodes_noNa=mean(minionNodes, na.rm=TRUE)
)


win.paramTimes <- ddply(win, c("essenceClass", "kind", "heuristic", "mode", "eprime", "paramHash", "run_no", "givenRunGroup"), summarise,
                   runTime=totalTime,
                   runTime_marked=totalTimeMarked,
                   nodes=minionNodes
)


yy <- ggplot( data=win.paramTimes, aes( x=paramHash, y=nodes, color=heuristic) )
yy <- yy + facet_grid(eprime ~ .)
yy <- yy + geom_point(shape=1)
# yy <- yy + geom_point(shape=1, position=position_jitter(width=0.1,height=0.01))
# yy <- yy + xlab("Model")
# yy <- yy + ylab("CPU Time")
yy <- yy + scale_y_log10()
yy


