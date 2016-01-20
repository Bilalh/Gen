our_path <- "~/CS/instancegen/scripts/analyse_results"
source( paste(our_path, "setup.r", sep='/'), chdir=TRUE)

# Finds params of the params are of increasing difficulty.


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


