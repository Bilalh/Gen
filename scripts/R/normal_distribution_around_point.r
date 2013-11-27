#!/usr/bin/env Rscript
# Generates sample points around with specified variance and means
# data_file is in the forum
# 10 20 
# 
library('plotrix')
library('mvtnorm')

args <- commandArgs(TRUE)

if (length(args) < 4){
  cat("Usage: normal_distribution_around_point.r num_samples variance data_file output_file [seed]\n")
  quit(status=10)
}

if (length(args) >= 5 ){
  cat(paste("Random seed is", args[5], "\n"))
  set.seed(as.numeric(args[5]))
}

num_samples = as.numeric(args[1])
# we can choose a variance arbitrarily at first, but fine tune this after sampling bouts...
exploring_variance = as.numeric(args[2])

data_file   = args[3]
output_file = args[4]

#num_samples <- 10
#exploring_variance = 30
#data_file   <- "in.txt"
#output_file <- "out.txt"

means <- read.table(data_file,col.names="")
cat("Input:\n")
means
cat("num_samples:",num_samples,"\n")


# using the mean for each and same variance.  Other methods could be used here...
circling = rmvnorm(num_samples,mean=means[,1],sigma=diag(c(exploring_variance,exploring_variance))) 

pdf('normal.pdf')
plot(circling,xlab="",ylab="",pch=1,cex=1,col="purple",axes=T,xlim=c(0,100),ylim=c(0,100))
draw.circle(means[1,1],means[2,1],exploring_variance/3,border="blue",lty=2,lwd=4)
dev.off()

cat("Results:\n")
circling

# Write results without headers
write.table(circling,file=output_file,quote=F,row.names=F,col.names=F)

