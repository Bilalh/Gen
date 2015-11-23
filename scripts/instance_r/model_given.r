list.of.packages <- c("plyr", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(plyr)
library(ggplot2)
library(scales)


if(! exists("prob")){

  # Run models.r to generate the cached results
  load("all_models.csv.bin")

  prob <- parts$prob013_PPP
  prob.title <- "Warehouse"

}


prob$paramUID <- as.factor(prob$paramUID)
prob$eprimeUID <- as.factor(prob$eprimeUID)


# We have run the params on multiple heuristics
mult <- prob[  ! is.na(prob$givenRunGroup)  , ]

# Only looking at the winning models
mult.win <- mult[ mult$isWinner ==1, ]

if (length(mult.win$essenceClass) == 0 ){
  stop("mult.win length 0")
}



# quantiles
quan.quality <-quantile(prob$paramQuality, c(0.1, 0.2, 0.5, 0.75, 1))
# quan.selected <- quan_quality[3]
quan.selected <- 1

# Only keep top n params

pq<-ddply(mult.win, c("paramUID"), summarise,  paramQuality=min(paramQuality, na.rm=TRUE) )

top_params <-  unique(pq[pq $paramQuality <=quan.selected ,  c("paramUID", "paramQuality")])
top_params.sorted <- top_params[order(top_params$paramQuality,top_params$paramUID),]
top_params.selected <- head(top_params.sorted,64)$paramUID
win <-  mult.win[  mult.win$paramUID %in% top_params.selected, ]

# Unfinished to max time
win$totalTimeMarked <- win$totalTime
win$totalTimeMarked[is.na(win$totalTimeMarked)] <- 600

win.avgtimes <- ddply(win, c("essenceClass", "kind", "heuristic", "mode", "eprimeUID", "run_no", "givenRunGroup"), summarise,
                   avgRunTime_noNa=mean(totalTime, na.rm=TRUE),
                   avgRunTime_marked=mean(totalTimeMarked),
                   minRunTime=min(totalTime, na.rm = TRUE),
                   maxRunTime=max(totalTime, na.rm = TRUE),
                   avgNodes_noNa=mean(minionNodes, na.rm=TRUE)
)

win.paramTimes <- ddply(win, c(
    "essenceClass", "kind", "heuristic", "mode", "eprimeUID", "paramUID", "run_no", "givenRunGroup"
  ), summarise,
  runTime = totalTime,
  runTime_marked = totalTimeMarked,
  nodes = minionNodes
  )



graph.by_class_model_param <- function(name){

  df <- ddply(win, c(
        "essenceClass", "kind", "heuristic", "mode", "eprimeUID", "paramUID", "run_no", "givenRunGroup"
      ), summarise,
      runTime = totalTime,
      runTime_marked = totalTimeMarked,
      nodes = minionNodes
    )

  yy <- ggplot( data=df, aes( x=paramUID, y=nodes, color=heuristic) )
  yy <- yy + facet_grid( eprimeUID ~ . )
  yy <- yy + geom_point(shape=1, size=I(3))
  # yy <- yy + geom_point(shape=1, position=position_jitter(width=0.1,height=0.01))
  yy <- yy + xlab("Param #")
  yy <- yy + scale_y_log10()
  yy <- yy + theme(legend.position="top")
  yy <- yy +  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  yy <- yy + ggtitle(paste(name, " winning models grouped by model #"))
  yy
}

graph.by_class_param_min <- function(name){

  df <- ddply(win, c(
    "essenceClass", "kind", "heuristic", "mode",  "paramUID", "givenRunGroup"
  ), summarise,
  runTime = min(totalTime,na.rm = TRUE),
  runTime_marked = min(totalTimeMarked),
  nodes = min(minionNodes,na.rm = TRUE)
  )


  yy <- ggplot( data=df, aes( x=paramUID, y=nodes, color=heuristic) )
  yy <- yy + geom_point(shape=1, size=I(3))
  # yy <- yy + geom_point(shape=1, position=position_jitter(width=0.1,height=0.01))
  yy <- yy + xlab("Param #")
  yy <- yy + scale_y_log10()
  yy <- yy + theme(legend.position="top")
  # yy <- yy + theme(axis.text.x= element_text(angle=45, hjust = 1.3, vjust = 1.2))
  yy <- yy +  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  yy <- yy + ggtitle(paste(name, ": ", "min nodes over all fractures", sep=""))
  yy
}

graph.base <- file.path("plots", "givens")
if (! dir.exists(graph.base) ){
  dir.create(graph.base,recursive = TRUE)
}
graph.save <- function(name,func){
  ggsave(file.path(graph.base, paste(name,"pdf",sep=".")), func, width = 15, height = 10)

}

# graph.by_class_model_param(prob.title)
# graph.by_class_param_min(prob.title)

graph.save(paste(prob.title, "param_model", sep="-"), graph.by_class_model_param(prob.title))
graph.save(paste(prob.title, "param_min",  sep="-"), graph.by_class_param_min(prob.title))
