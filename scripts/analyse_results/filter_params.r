our_path <- "~/CS/instancegen/scripts/analyse_results"
source( paste(our_path, "setup.r", sep='/'), chdir=TRUE)

get_discriminating_params <- function(essence_name){
  a<-sall[sall$essence==essence_name, ]
  st.idx <- a$index
  st.params <- every_param[every_param$index %in% st.idx, ]
  st.params <- st.params[ st.params$quality < 1,  ]

  st.params
}

get_discriminating_params_by_method <- function(essence_name, method){
  st.idx <- sall[sall$essence==essence_name, ]
  st.idx <- st.idx[st.idx$method==method, ]

  st.params <- every_param[every_param$index %in% st.idx$index, ]
  st.params <- st.params[ st.params$quality < 1,  ]

  st.params
}

save_names <- function(essence, st.params, suffix="", dirname='params', limit=3){
  st.sat   <- st.params[st.params$MaxSolutions >= 1,  ]
  st.unsat <- st.params[st.params$MaxSolutions == 0,  ]

  process <- function(picked, filename){
    pickedM <- merge(by='index', x=picked,
                     y=sall[c('index', 'output_dir', 'method_opts', 'method_opts2',
                              'models_timeout', 'method', 'num_models', 'essence', 'mode', 'run_no') ])

    pickedM$model_timeout <- pickedM$models_timeout / pickedM$num_models

    #pickedU <- pickedM[!duplicated(pickedM[,c('essence', 'method', 'eprimesLeft')]),]
    pickedU <- pickedM
    pickedU <- pickedU[order(pickedU$quality, decreasing=FALSE), ]
    pickedU <- head(pickedU, n=limit)

    filedir <- paste(base, dirname, essence, sep='/' )
    filepath <- paste(filedir, filename , sep='/' )
    dir.create(filedir, recursive=TRUE)

    write.csv(pickedU , file=paste0(filepath,suffix, ".csv" ) )
    write.csv(pickedU[, c('essence', 'num_models', 'Satisfiable', 'method',  'run_no',  'eprimesLeft', 'eprimes',  'quality',
                          'ordering',  'MaxSolutions', 'mode', 'paramHash') ] , file=paste0(filepath,suffix, "-short.csv" ) )

#     write.table(pickedM$paramHash, file=paste0(filepath, "_names.txt" ),   quote=FALSE, col.names=FALSE, row.names=FALSE)
    a<-pickedU[ c('paramHash', 'eprimes', 'output_dir', 'mode', 'method') ]
    a$e2 <-  sapply( a$eprimes ,function(x)  gsub(", ", ",", x)   )

    write.table(a[ c('paramHash', 'output_dir', 'mode', 'method', 'e2') ] , file=paste0(filepath, suffix, "_names2.txt" ),
                quote=FALSE, col.names=FALSE, row.names=FALSE)


  }



  process(st.sat, "sat" )
  process(st.unsat, "unsat")

}

ems <- unique( sall[ c('essence', 'method') ] )

for (i  in 1:length(ems$essence) ){
  save_names( ems[i,]$essence, get_discriminating_params_by_method(ems[i,]$essence,  ems[i,]$method ),
              paste0("-",ems[i,]$method))
}

for (u  in  unique(sall$essence) ){
  save_names(u, get_discriminating_params(u), dirname="params_all_methods", limit=50 )
}

