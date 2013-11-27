# From Glenna

#----------------------
# Space exploration demo
# You'll need to download and install two R packages: 'plotrix' and 'spatstat'
#-----------------------

# Needed these lines
library('plotrix')
library('spatstat')
library('mvtnorm')

# building frame containing areas of exploration

x = runif(10000,0,1) # the limits 0 and 1 are arbitrarily chosen
y = runif(10000,0,1)
  
plot(x,y,xlab="x",ylab="y",main="Exploration space",col="grey",pch=16) # plotit


# adding the good and bad regions
# NB: good regions are inside the circles

plot(x,y,xlab="x",ylab="y",main="Exploration space",col="grey",pch=16)
draw.circle(0.3,0.4,.2,border="blue",lty=2,lwd=4)
par(new=TRUE)
draw.circle(0.4,0.6,.2,border="blue",lty=2,lwd=4)
par(new=TRUE)
draw.circle(0.7,0.1,.05,border="blue",lty=2,lwd=4)

#  Randomly chosing exploratory sample sites or parent points

Sample_sitesx = runif(15,0,1)
Sample_sitesy = runif(15,0,1)


# Viewing sample_sites in exploration space

plot(x,y,xlab="x",ylab="y",main="Exploration space",col="grey",pch=16,xlim=c(0,1),ylim=c(0,1))
draw.circle(0.3,0.4,.2,border="blue",lty=2,lwd=4)
par(new=TRUE)
draw.circle(0.4,0.6,.2,border="blue",lty=2,lwd=4)
par(new=TRUE)
draw.circle(0.7,0.1,.05,border="blue",lty=2,lwd=4)
par(new=TRUE)
plot(Sample_sitesx,Sample_sitesy,xlab="",ylab="",pch=16,cex=3,col="red",axes=F)

# If no good areas are obtained resample again with a blindfold on...or resample from the space with
# the previous bad zones removed.

# If there are good zones.....filter out the good zones
# So, say we found a good zone with coordinates x = 0.3, y = 0.4

filterx = 0.3
filtery = 0.4
exploring_variance = 0.01 # we can choose a variance arbitrarily at first, but fine tune this after sampling bouts...


# simulating two values...x and y
circling_goodzone = rmvnorm(50,mean=c(filterx,filtery),sigma=diag(c(exploring_variance,exploring_variance))) 
                                 

# using the mean for each and same variance.  Other methods could be used here...
plot(x,y,xlab="x",ylab="y",main="Exploration space",col="grey",pch=16,xlim=c(0,1),ylim=c(0,1))
draw.circle(0.3,0.4,.2,border="blue",lty=2,lwd=4)
par(new=TRUE)
draw.circle(0.4,0.6,.2,border="blue",lty=2,lwd=4)
par(new=TRUE)
draw.circle(0.7,0.1,.05,border="blue",lty=2,lwd=4)
par(new=TRUE)
plot(Sample_sitesx,Sample_sitesy,xlab="",ylab="",pch=16,cex=3,col="red",axes=F)
par(new=TRUE)
plot(circling_goodzone,xlab="",ylab="",pch=16,cex=2,col="purple",axes=F,xlim=c(0,1),ylim=c(0,1))

# Use daughter points to get more good points.
# Backtrack if no good zones were found




#------------ Doing this more elegantly using point patterns (you need to install the 'spatstat' R package)


# SAMPLING IN CLUSTERS instead of single points

# homogeneous
plot( rMatClust(2, 0.1, 2), win = owin(c(1,100),c(1,100)))  
#simulating points (for more formally, a 'point pattern' with parent points of intensity 10, radius of cluster
#r, mean number of cluster points and importantly the window from which to simulate.  The theory behind this
#is that the parent points are generated from a Poisson point process.  The assumption for this process is
#that the points are independent and randomly distributed in t he window.  This is like a 'null' hypothesis:
#'the good points are randomly distributed'?

 # inhomogeneous
 Z <- as.im(function(x,y){ 4 * exp(2 * x - 1) }, owin())

plot( rMatClust(10, 0.05, Z)) # adding inhomogeneity in the pattern of points 

plot(rGaussPoisson(30, 0.07, 0.5))
# simulating points with parents points are completely spatially randomly distributed (the pattern exhibits
# CSR) and then adding daughter points with around them.  Similar to the two step procedure described at the
# beginning of this toy script.




# MORE STUFF...
 # each cluster consist of 10 points in a disc of radius 0.2
  nclust <- function(x0, y0, radius, n) {
              return(runifdisc(n, radius, centre=c(x0, y0)))
            }
  plot(rNeymanScott(10, 0.2, nclust, radius=0.2, n=5))


  # multitype Neyman-Scott process (each cluster is a multitype process)
  nclust2 <- function(x0, y0, radius, n, types=c("a", "b")) {
     X <- runifdisc(n, radius, centre=c(x0, y0))
     M <- sample(types, n, replace=TRUE)
     marks(X) <- M
     return(X)
  }
  plot(rNeymanScott(15,0.1,nclust2, radius=0.1, n=5))

 
 # other possibilities...
 #--------------------------
  #  at the end of the sampling process, the end product could be a point pattern of good zones 
  #  where the good zones are discretized into tiny points.
  
 
  # birth death algorithms (with MCMC)  can be used to simulated the point pattern of good zones 
  # if you have an idea of the intensity of good zones.
  
  # sampling strategies...line transect, stratified, random sampling, systemmatic, cluster
  # 68,95,99.7 rule  for Normal distribution would be good to know
  
  #  calculate the power of this sampling methods...ie how does it compare to just random uniform sampling...