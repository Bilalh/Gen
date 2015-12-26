list.of.packages <- c("plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(plyr)

base <- path.expand("~/Desktop/Results/sampling_no_large/")
all <- read.csv(file.path(base, "all.csv"))

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
           hostType=toString(hostType),
           seq=toString(seq),
           kind =toString(kind),
           rCPUTime=mean(rCPUTime),
           rRealTime=mean(rRealTime),
           fCPUTime=mean(rCPUTime)/3600,
           fRealTime=mean(rRealTime)/3600,
           influence_radius=toString(influence_radius),
           compact=toString(compact),
           fractures=toString(fractures),
           end=""
           )


finished=info[info$runs==3,]
write.csv(finished, file="summary.csv")


names.essence <- unique(all$essenceClass)
# selected.ppp  <- all[ all$essenceClass=="prob013-PPP"       &  all$kindClass=="undirected" & all$mode=="sample-64" , ]
# selected.wh   <- all[ all$essenceClass=="prob034-warehouse" &  all$kindClass=="undirected" & all$mode=="sample-64" , ]
# selected.bacp <- all[ all$essenceClass=="prob030-BACP"      &  all$kindClass=="undirected" & all$mode=="sample-64" , ]
# selected.efpa <- all[ all$essenceClass=="prob055-efpa"      &  all$kindClass=="undirected" & all$mode=="sample-64" , ]
# selected.sgp  <- all[ all$essenceClass=="prob010-SGP"       &  all$kindClass=="undirected" , ]
#
# info.ppp  <- info[ info$essenceClass=="prob013-PPP" , ]
# info.wh   <- info[ info$essenceClass=="prob034-warehouse" , ]
# info.sgp  <- info[ info$essenceClass=="prob010-SGP" , ]

info2 <- info[ ! info$refineGroup  %in%  c("2015-11-06_symlink", "2015-11-25") , ]
info3 <- info2[ ! info2$group %in% c(16044, 14016, 12017, 12019, 12020, 12018, 16045), ]


# View(info)
# View(info2)
View(info3)
