#!/usr/bin/env Rscript

# Reads table in the form 
#   V1 V2
# 1  1 10
# 2  2  8
# 3  9 32
# Where V1 is the lower bound and V2 is the upper bound

args <- commandArgs(TRUE)

if (length(args) < 3){
  cat("Usage: uniform_sample.r num_samples data_file output_file [seed]\n")
  quit(status=10)
}

if (length(args) >= 4 ){
  cat(paste("Random seed is", args[4], "\n"))
  set.seed(as.numeric(args[4]))
}

num_samples = as.numeric(args[1])
data_file   = args[2]
output_file = args[3]



# num_samples <- 5
# data_file   <- "a.txt"
# output_file <- "b.txt"

r <- read.table(data_file)
cat("Input:\n")
r
num_samples

# Returns uniform sample distribution num_samples times
uniformSample <- function(bounds){
  sample(bounds[1]:bounds[2],size=num_samples,replace=T)
}

# Get the resultd in rows 
rows_ps <- apply(r, MARGIN=1, uniformSample)

cat("Results:\n")
rows_ps

# Write results without headers
write.table(rows_ps,file=output_file,quote=F,row.names=F,col.names=F)

