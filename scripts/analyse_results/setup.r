# for rename
require(reshape)
require(tables)

# filepaths
base <- paste("~/Desktop/Experiment" , "_all_sols", sep='/')
f_all = paste(base, "all.csv",  sep='/')
f_param_info = paste(base, "extra_data/param_eprime_info.csv",  sep='/')
f_every_param = paste(base, "extra_data/every_param.csv",  sep='/')

# Load data
param_info <- read.csv(f_param_info)    # param/eprime summary 
every_param <- read.csv(f_every_param)  # every param

read_configs <- function(){
  all <- read.csv(f_all)                  # All configs of the algorithms
  # quality is in the range (0, 1.0 ], if an algorithms found nothing call it 1 
  all$best_quality[is.na(all$best_quality)] <- 1
  
  # Add the resultsing models
  all$min_models <- all$best_quality * all$num_models
  
  all$point_selector[all$point_selector==""] <- NA
  
  # method with options
  method_specific= c("method", 'chain_length', "num_points", "point_selector", "influence_radius", "radius_as_percentage", "use_minion", "pre_generate", "iterations")
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
  
  sall
}

sall <- read_configs()