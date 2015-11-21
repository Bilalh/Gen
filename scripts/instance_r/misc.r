library(plyr)
dt <- data.frame(age=rchisq(20,10),group=sample(1:2,20,replace=T))
z<-ddply(dt,~group,summarise,mean=mean(age),sd=sd(age),asas=max(age))


data(ChickWeight)
c<-split( ChickWeight , ChickWeight$Diet )


x <- c(3:5, 11:8, 8 + 0:5)
(ux <- unique(x))
