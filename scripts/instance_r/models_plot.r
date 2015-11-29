# Old plots

# Run models.r to generate the cached results
load("all_models.csv.bin")

list.of.packages <- c("plyr", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(plyr)
library(ggplot2)
library(scales)

prob<-parts$prob034_warehouse

paramHashes <- c(
                "05d21c8d1fe5e0b1b18bdbe1d31064d067a210de7387d539d7121bf4b511e130007844dfae00388921f06b19af4835eeafa8f673b0cb86727991fd20e8afc22d"
       ,         "40112dee37233d1aa32740a51ea873dbb21f8d37205d3b0734495836a46ef1494979715baf429b63dbd072bec39a678d8e3f5650178da0b587b719a447e8bd94"
        ,        "49bc919b3c693b077f8139c68057ccb412d12c12a8f04df3c3ab98d7082f5f622f7110c5997f51eeec980b46971d70730609bec33b86f70680c6133b920a2620"
         ,       "cda42572b122ed49d7ec202a7acc162a57369c26cd97941c579d0130494aefc03c154810c69988d2319a4ea93436ab613f0ac8eb2aed18935728d91d5c935b6b"
          ,      "dc8cb9ed3bc0020fd9dc17594379ed91063edffb6e8943927367dead8377fdca1c9f508f7c2c8d74877ca766d3aa70bd6718504ab5683b8aea76b7d571dc0707"
           ,     "40112dee37233d1aa32740a51ea873dbb21f8d37205d3b0734495836a46ef1494979715baf429b63dbd072bec39a678d8e3f5650178da0b587b719a447e8bd94"
                )
df <- prob[ prob$paramQuality < 1
          & prob$kind %in% c("undirected~given", "undirected")
          & prob$seq %in% c(16026, 16001)
          & prob$paramHash %in% paramHashes
         , ]

df$totalTimeMarked <- df$totalTime
df$totalTimeMarked[is.na(df$totalTimeMarked)] <- 600


pf <- ddply(df, c("seq", "paramHash", "eprimeId", "heuristic"), summarise,
            avgTime=mean(totalTimeMarked)
            # avgTime=mean(totalTime, na.rm = TRUE)
            # nodes=minionNodes
           )

head(pf)
# seq is not an conn int
pf$seq <- as.factor(pf$seq)
# pf$eprimeId <- as.factor(pf$eprimeId)

# pf.sdf <- pf[pf$seq==16026,]
# pf.static <- pf[pf$seq==16001,]


yy <- ggplot( data=pf, aes( x=eprimeId, y=avgTime, color=heuristic) )
yy <- yy + geom_point(shape=1, position=position_jitter(width=0.1,height=0.01))
yy <- yy + xlab("Model No.") + ylab("CPU Time")
# yy <- yy + scale_y_log10()
yy
