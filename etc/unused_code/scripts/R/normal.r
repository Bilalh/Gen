#!/usr/bin/env Rscript
library(MASS)

Sigma <- matrix(c(10,3,3,2),2,2)
Sigma <- matrix(rep(0,9),3,3)
Sigma <- matrix(c(1,0,0, 1,0,0, 1,0,0),3,3)
Sigma

r <- mvrnorm(n=5, c(100,40,20), Sigma)


n=10
p=.5
x=0:10
p=dbinom(x,size=n,prob=p)
plot(x,p,type="h",xlim=c(-1,11),ylim=c(0,0.5),lwd=2,col="blue",ylab="p")
points(x,p,pch=16,cex=2,col="dark red")


rmultinom(10, size = 12, prob = c(0.1,0.2,0.8))

pr <- c(1,3,6,10) # normalization not necessary for generation
rmultinom(10, 20, prob = pr)

## all possible outcomes of Multinom(N = 3, K = 3)
X <- t(as.matrix(expand.grid(0:3, 0:3))); X <- X[, colSums(X) <= 3]
X <- rbind(X, 3:3 - colSums(X)); dimnames(X) <- list(letters[1:3], NULL)
X
round(apply(X, 2, function(x) dmultinom(x, prob = c(1,2,5))), 3)