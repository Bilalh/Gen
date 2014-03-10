our_path <- "~/CS/instancegen/scripts/analyse_results"
source( paste(our_path, "setup.r", sep='/'), chdir=TRUE)

get_discriminating_params <- function(essence_name){
  st.idx <- sall[sall$essence==essence_name, ]$index
  st.params <- every_param[every_param$index %in% st.idx, ]
  st.params <- st.params[ st.params$quality != 1,  ]
  
  st.params
}

save_names <- function(essence, st.params){
  st.sat   <- st.params[st.params$MaxSolutions >= 1,  ]
  st.unsat <- st.params[st.params$MaxSolutions == 0,  ]
  
  process <- function(picked, filename){
    pickedM <- merge(by='index', x=picked, 
                     y=sall[c('index', 'output_dir', 'method_opts', 'method_opts2', 
                              'models_timeout', 'method', 'num_models', 'essence', 'mode') ])
    
    pickedM$model_timeout <- pickedM$models_timeout / pickedM$num_models
    
    filedir <- paste(base, "params", essence, sep='/' )
    filepath <- paste(filedir, filename , sep='/' )
    dir.create(filedir, recursive=TRUE)
    
    write.csv(pickedM , file=paste0(filepath, ".csv" ) )
#     write.table(pickedM$paramHash, file=paste0(filepath, "_names.txt" ),   quote=FALSE, col.names=FALSE, row.names=FALSE)
    a<-pickedM[ c('paramHash', 'eprimes', 'output_dir', 'mode', 'method') ]
    a$e2 <-  sapply( a$eprimes ,function(x)  gsub(", ", ",", x)   ) 
    View(a)
    
    write.table(a[ c('paramHash', 'output_dir', 'mode', 'method', 'e2') ] , file=paste0(filepath, "_names2.txt" ), quote=FALSE, col.names=FALSE, row.names=FALSE)
    
    
  }
  
  process(st.sat, "sat")
  process(st.unsat, "unsat")
  
}
  
discm <- get_discriminating_params('prob038-steel')
save_names('prob038-steel', discm)



