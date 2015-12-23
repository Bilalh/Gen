# Run models.r to generate the cached results

list.of.packages <- c("plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(plyr)

base <- path.expand("~/Desktop/Results/sampling_no_large/")

# Cache the data for quick loading
# Delete all_models.csv.bin to refresh the data
if (file.exists("all_models.csv.bin")) {
  load("all_models.csv.bin")
}else{
  models_ <- read.csv(file.path(base,"all_models.csv"))
  p_id <- data.frame(paramHash= unique(models_$paramHash), paramUID = seq(length(unique(models_$paramHash))) )
  e_id <- data.frame(eprime= unique(models_$eprime), eprimeUID = seq(length(unique(models_$eprime))) )
  models <- merge(merge(models_,p_id, by.x = "paramHash", by.y = "paramHash"), e_id, by.x="eprime", by.y="eprime"  )

  models$paramUID <- as.factor(models$paramUID)
  models$eprimeUID <- as.factor(models$eprimeUID)

  parts <- split( models , models$essenceClass )
  pnames <- names(parts)
  var_names <- gsub("-","_", pnames)
  names(parts) <- var_names

  models2 <- models[ ! models$refineGroup  %in%  c("2015-11-06_symlink", "2015-11-25") , ]
  models2 <- models2[ ! models2$group %in% c(16044, 14016, 12017, 12019, 12020, 12018, 16045), ]

  parts2 <- split( models2 , models2$essenceClass )
  pnames2 <- names(parts2)
  var_names2 <- gsub("-","_", pnames2)
  names(parts2) <- var_names2

  save(models, parts, models2, parts2, file="all_models.csv.bin", compress=FALSE)
}

# Make each class it's own data frame
# list2env(parts, envir = .GlobalEnv)


# examples

# The problem to look at
p<-parts$prob034_warehouse

ex <- p[ p$isWinner ==1
       & p$paramQuality < 1
       , ]
(unique(ex$eprime))

ex$totalTimeMarked <- ex$totalTime
ex$totalTimeMarked[is.na(ex$totalTimeMarked)] <- 600

ex.times <- ddply(ex, c("essenceClass", "kind", "heuristic", "mode", "eprime", "run_no"), summarise,
      avgRunTime_noNa=mean(totalTime, na.rm=TRUE),
      avgRunTime_marked=mean(totalTimeMarked),
      minRunTime=min(totalTime, na.rm = TRUE),
      maxRunTime=max(totalTime, na.rm = TRUE),
      avgNodes_noNa=mean(minionNodes, na.rm=TRUE)
)

head(ex.times)
# View(ex.times)


