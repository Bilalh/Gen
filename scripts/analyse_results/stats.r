our_path <- "~/CS/instancegen/scripts/analyse_results"
source( paste(our_path, "setup.r", sep='/'), chdir=TRUE)
source( paste(our_path, "tabular.cast_df.r", sep='/'), chdir=TRUE)



#View(sall[which(method =='nsample' & use_minion==0 & influence_radius ==10 & races == 1),])

#require("data.table")
#dt = data.table(sall)
#dt[, list(best_quality, mean=mean(dt) ), by=c('races', 'total_timeout')]

# Plot subplots of races,quality, method
d <- subset(sall, select=c(races,best_quality, method) )
plot(d)

# aggregate param by quality 
ey <- aggregate(every_param$quality, by=list(every_param$index), FUN=min)
ey <- rename(ey,c("Group.1"='index', "x" = 'quality' ))
ey_sorted <- ey[order(ey$quality), ]

# other aggregations 
aggregate(sall$best_quality ~ sall$races + sall$influence_radius + sall$method  , data=sall, FUN=mean, na.action=na.pass) 



# Produce a summary of the results 
m<- melt(sall,id.vars=c('essence', 'method', 'total_timeout', 'races'), measure.vars=c("min_models") )
sum <- tabular(cast(m, races ~ method, c(mean,sd,min, max), margins=TRUE))

# Produce a summary of the results per method
m2<- melt(sall,id.vars=c('essence', 'method', 'method_opts2', 'total_timeout', 'races'), measure.vars=c("min_models") )
#stats <- tabular(cast(m2, races ~ method_opts2, c(mean,sd,min, max), margins=TRUE))


library(ggplot2) 

# qplot(races, min_models, data=all[all$method == 'ksample',], shape=method, color=method, 
#       facets=point_selector~essence, size=I(3),
#       ylab="Number of models left" ) + ylim(0, 100)

# Plots
p <- qplot(races, min_models, data=sall, shape=method, color=method, 
           facets=run_no~use_minion, size=I(3),
           xlab="Races", ylab="Number of models left") 
p + ggtitle("Using Minion to genrate params/run_no") + ylim(0,100)

# to make output prettier
longNamedFuncbl <- function (x){ NA }
sp <- function(x){ NA }

per_method <- function (method_name){
  temp <- m2[ m2$method == method_name,   ]
  tabular(cast(temp, essence + races ~ method_opts2 , 
              c(longNamedFuncbl, mean,sd,min, max), 
         margins=TRUE, fill=NA))
  
}

per_essence <- function (essence_name){
  temp <- m2[ m2$essence == essence_name,   ]
  cols = cast(temp, method_opts2 + races ~  essence, 
              c(longNamedFuncbl, mean,sd,min, max), 
              margins=TRUE, fill=NA)
  tabular(cols[, -grep("^\\(all", colnames(cols))])
  
}

old_width <- getOption("width")
options(width=500)

sink(paste(base, "stats_by_method.txt", sep='/'),split=TRUE)
lapply( unique(m2$method), per_method)
sink(NULL)

sink(paste(base, "stats_by_essence.txt", sep='/'),split=TRUE)
lapply( unique(m2$essence), per_eseence)
sink(NULL)

options(width=old_width)



temp <- m2[ m2$essence == "prob024-Langford",   ]
cols = cast(temp, method_opts2 + races ~  essence, 
            c(longNamedFuncbl, mean,sd,min, max), 
            margins=c('ksample,100,halving_3_4, 4, 0,0  '), fill=NA)
cols2<- cols[, -grep("^\\(all", colnames(cols))]
cols2 <- cols2[-nrow(cols2),]
tabular(cols)


df <- data.frame(ID = 1:10,
                 A1 = rnorm(10),
                 A2 = rnorm(10),
                 B1 = letters[1:10],
                 B2 = letters[11:20])
df[, -grep("A", colnames(df))]
