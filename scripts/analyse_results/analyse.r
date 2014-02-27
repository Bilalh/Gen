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
method_specific= c("method", 'chain_length', "num_points", "point_selector", "influence_radius", "radius_as_percentage", "use_minion")
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


# to make output prettier
longNamedFuncbl <- function (x){ NA }
sp <- function(x){ NA }

per_method <- function (method_name){
  temp <- m2[ m2$method == method_name,   ]
  tabular(cast(temp, essence + races ~ method_opts2 , c(longNamedFuncbl, mean,sd,min, max), margins=TRUE, fill=NA))
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


#  all knapsack params group by sat/unsat
ks.idx <- sall[sall$essence=="prob133-knapsack3", ]$index
ks.params         <- every_param[every_param$index %in% ks.idx, ]
ks.sat            <- ks.params[ks.params$MaxSolutions >= 1,  ]
ks.unsat          <- ks.params[ks.params$MaxSolutions == 0,  ]


A <- 10 * (1:5)
b <- c(13, 17, 20)
sapply(b,function(x)which.min(abs(x - A)))


# Make powers of two,  then convert it in the range 0:1  
# then add few extra values to the end 
# then reserve them
qualities <-  2 ^ (1:6) / 100
(qualities <- c(qualities, 0.8, 0.9, 1.0))
(qualities <- 1 - qualities)

get_picked <- function(qs, vals){
  ( vals.picked.idx <- sapply(qs,function(x)which.min(abs(x - vals$quality )))  )
  (vals.picked.idx  <-  unique(vals.picked.idx))
  vals.picked <- vals[vals.picked.idx, ]
  vals.pickedM <- merge(by='index', x=vals.picked, 
    y=sall[c('index', 'output_dir', 'method_opts', 'method_opts2', 'models_timeout', 'method', 'num_models', 'essence') ])
  
  vals.pickedM$model_timeout <- vals.pickedM$models_timeout / vals.pickedM$num_models
  vals.pickedM
}

ks.sat.picked <-  get_picked(qualities, ks.sat)
ks.unsat.picked <-  get_picked(qualities, ks.unsat)

ks.sat_csv   <-  paste(base, "ks_sat.csv",  sep='/')
ks.unsat_csv <-  paste(base, "ks_unsat.csv",  sep='/')
ks.sat_names   <-  paste(base, "ks_sat_names.txt",  sep='/')
ks.unsat_names <-  paste(base, "ks_unsat_names.txt",  sep='/')

write.csv(ks.sat.picked, file=ks.sat_csv)
write.csv(ks.unsat.picked, file=ks.unsat_csv)

write.table(ks.sat.picked$paramHash,   file=ks.sat_names,   quote=FALSE, col.names=FALSE, row.names=FALSE)
write.table(ks.unsat.picked$paramHash, file=ks.unsat_names, quote=FALSE, col.names=FALSE, row.names=FALSE)


