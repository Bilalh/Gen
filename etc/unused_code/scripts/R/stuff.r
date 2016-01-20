library('plotrix')
library('spatstat')
library('mvtnorm')


filterx = 50
filtery = 40
exploring_variance = 30 # we can choose a variance arbitrarily at first, but fine tune this after sampling bouts...

circling_goodzone = rmvnorm(10,mean=c(filterx,filtery),sigma=diag(c(exploring_variance,exploring_variance))) 

# using the mean for each and same variance.  Other methods could be used here...

plot(circling_goodzone,xlab="",ylab="",pch=1,cex=1,col="purple",axes=T,xlim=c(0,100),ylim=c(0,100))
draw.circle(50,40,10,border="blue",lty=2,lwd=4)
circling_goodzone