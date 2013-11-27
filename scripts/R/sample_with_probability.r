#!/usr/bin/env Rscript
# Get n samples without repeats using the specifed probabilities

# Using a  matrix (or vector) with probabilites      get n samples without repeats
#        [,1] [,2] [,3]       [,1] [,2] [,3]          [,1]
# [1,]    1    2    3   [1,]  0.1  1.5    0    [1,]    2
# [2,]    4    5    6   [2,]  1.0  1.5    0    [2,]    8
# [3,]    7    8    9   [3,]  0.5  1.5    0    [3,]    7

args <- commandArgs(TRUE)

if (length(args) < 3){
  cat("Usage: sample_with_probability.r num_samples data_file output_file [seed]\n")
  quit(status=10)
}

if (length(args) >= 4 ){
  cat(paste("Random seed is", args[4], "\n"))
  set.seed(as.numeric(args[4]))
}

num_samples = as.numeric(args[1])
data_file   = args[2]
output_file = args[3]



#num_samples <- 5
#data_file   <- "p.txt"
#output_file <- "q.txt"

probs <- read.table(data_file)
cat("Input:\n")
probs
cat("num_samples:",num_samples,"\n")

if (num_samples > length(probs)){
  cat("Error: number of sample greater then number of probabilities\n")
  quit(status=11)
}

res <- sample(1:length(probs), size=num_samples, prob=probs,replace=F)
res <- sapply(res,function(x){ x -1 })

cat("Results:\n")
res

# Write results without headers
write.table(res,file=output_file,quote=F,row.names=F,col.names=F)
