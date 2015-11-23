list.of.packages <- c("plyr", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(plyr)
library(ggplot2)
library(scales)


# Run models.r to generate the cached results
load("all_models.csv.bin")

u<-unique( models$essenceClass )
for(name in u) {
  prob <- models[ models$essenceClass == name, ]
  prob.title <- name
  print(prob.title)

  tryCatch(
    source("model_given.r"),
    error = function(e)
    {
      print(paste("ERROR:", name, e$message))
    }
  )

}


