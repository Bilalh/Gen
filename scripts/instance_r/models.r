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
  models <- read.csv(file.path(base,"all_models.csv"))

  pnames <- unique(models$essenceClass)
  var_names <- gsub("-","_", pnames)

  parts <- split( models , models$essenceClass )
  names(parts) <- var_names
  save(models, parts, file="all_models.csv.bin", compress=FALSE)
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


