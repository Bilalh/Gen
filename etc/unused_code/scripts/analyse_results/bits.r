# random bits of r code, that sometimes useful

# aggregate
col_names <- colnames(all)
agg_names <- list(
  all$method, all$essence, all$total_timeout, all$races,  all$point_selector, all$influence_radius
)

c("best_quality", "chain_length", "essence", "index", "influence_radius", 
  "max_discriminating", "method", "models_timeout", "num_models", 
  "num_points", "param_count", "point_selector", "races", "radius_as_percentage", 
  "run_no", "total_timeout", "output_dir")
#g<-aggregate(all, by=agg_names, FUN=mean)
f<-subset(all, best_quality < 0.1   )


# ggplot2 examples
library(ggplot2) 
attach(mtcars)
# create factors with value labels 
mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),
                      labels=c("3gears","4gears","5gears")) 
mtcars$am <- factor(mtcars$am,levels=c(0,1),
                    labels=c("Automatic","Manual")) 
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),
                     labels=c("4cyl","6cyl","8cyl")) 

# Scatterplot of mpg vs. hp for each combination of gears and cylinders
# in each facet, transmittion type is represented by shape and color
qplot(hp, mpg, data=mtcars, shape=am, color=am, 
      facets=gear~cyl, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon") 
