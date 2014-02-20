# for rename
require(reshape)
require(tables)

setwd("~/CS/instancegen/scripts/analyse_results")
source("tabular.cast_df.r")

base <- paste("~/Desktop/Experiment" , "_data_azure", sep='/')
f_all = paste(base, "all.csv",  sep='/')
f_param_info = paste(base, "extra_data/param_eprime_info.csv",  sep='/')
f_every_param = paste(base, "extra_data/every_param.csv",  sep='/')

# Load data
all <- read.csv(f_all)                  # All configs of the algorithms
param_info <- read.csv(f_param_info)    # param/eprime summary 
every_param <- read.csv(f_every_param)  # every param

# quality is in the range (0, 1.0 ], if an algorithms found nothing call it 1 
all$best_quality[is.na(all$best_quality)] <- 1

# Add the resultsing models
all$min_models <- all$best_quality * all$num_models

all$point_selector[all$point_selector==""] <- NA

# method with options
method_specific= c("method", 'chain_length', "num_points", "point_selector", "influence_radius", "radius_as_percentage")
all$method_opts <-apply(all[method_specific], 1, paste, collapse=",")
all$method_opts2 <- gsub(",NA", "", all$method_opts, perl=TRUE)

# Number the groups, a group has the same config
all$output_dir2 <- sub("__\\d", "", all$output_dir, perl=TRUE )
group_ids <- rep(NA, length(all$index))
u <-  unique(all$output_dir2)
for (i in (1:length(u) )  ){ 
  for( ix in all[all$output_dir2 == u[i], ]$index){
    group_ids[ix+1] = i # R is 1 indexed but index is [0..]
  }
}

# sort by index
sall <- all[order(all$index), ]
sall$group_num <- group_ids


# Plot subplots of races,quality, method
d <- subset(all, select=c(races,best_quality, method) )
plot(d)

# aggregate param by quality 
ey <- aggregate(every_param$quality, by=list(every_param$index), FUN=min)
ey <- rename(ey,c("Group.1"='index', "x" = 'quality' ))
ey_sorted <- ey[order(ey$quality), ]

# other aggregations 
aggregate(sall$best_quality ~ sall$races + sall$influence_radius + sall$method  , data=sall, FUN=mean, na.action=na.pass) 


# Produce a summary of the results 
m<- melt(sall,id.vars=c('essence', 'method', 'total_timeout', 'races'), measure.vars=c("min_models") )
sum <- tabular(cast(m, races ~ method, c(mean,sd,min, max), margins=TRUE))

# Produce a summary of the results per method
m2<- melt(sall,id.vars=c('essence', 'method', 'method_opts2', 'total_timeout', 'races'), measure.vars=c("min_models") )
#stats <- tabular(cast(m2, races ~ method_opts2, c(mean,sd,min, max), margins=TRUE))

per_method <- function (method_name){
  temp <- m2[ m2$method == method_name,   ]
  tabular(cast(temp, essence + races ~ method_opts2 , c(mean,sd,min, max), margins=TRUE, fill=NA))
}
stats <- lapply( unique(m2$method), per_method)

library(ggplot2) 

# qplot(races, min_models, data=all[all$method == 'ksample',], shape=method, color=method, 
#       facets=point_selector~essence, size=I(3),
#       ylab="Number of models left" ) + ylim(0, 100)

# Plots
p <- qplot(races, min_models, data=all, shape=method, color=method, 
      facets=run_no~use_minion, size=I(3),
      xlab="Races", ylab="Number of models left") 
p + ggtitle("Using Minion to genrate params/run_no") + ylim(0,100)


#View(all[which(method =='nsample' & use_minion==0 & influence_radius ==10 & races == 1),])

#require("data.table")
#dt = data.table(sall)
#dt[, list(best_quality, mean=mean(dt) ), by=c('races', 'total_timeout')]
