our_path <- "~/CS/instancegen/scripts/analyse_results"
source( paste(our_path, "setup.r", sep='/'), chdir=TRUE)
library(ggplot2) 
library(ggthemes)

`%ni%` = Negate(`%in%`)
  
essences_= c('prob013-PPP', 'prob034-Warehouse')
#method_opts2_=c('nsample, 4, 0,1', "uniform,0", "ksample,100,halving_3_4,10, 0,0")e
method_opts2_=c('nsample, 4, 0,1,0,30', 'uniform,1,0,30', 'smac,0,0', 'nsample,10, 0,1,0,30')
races_=c(5, 0,30)
group_num_neg=c(22)

es <- subset(sall, essence %in% essences_  )
plot_data <- subset(sall, method_opts2 %in% method_opts2_ & essence %in% essences_ & races %in% races_ & group_num %ni% group_num_neg )

q <- qplot(method, min_models, data=plot_data, size=I(3)  )
q + 
  facet_wrap(~ essence , ncol=2, scales="free_y") + 
  theme_bw(base_size = 12, base_family = "Helvetica") +
  geom_point(position = position_jitter(w = 0.05)) +
  ggtitle("Number of models left on the most discriminating instance (192 models)")
