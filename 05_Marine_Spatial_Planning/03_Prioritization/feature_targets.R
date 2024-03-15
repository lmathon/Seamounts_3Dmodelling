library(ggplot2)
library(tidyverse)
library(ggpubr)


load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization10/Solution10_blm_0/feature_target10.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization20/Solution20_blm_0/feature_target20.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization30/Solution30_blm_0/feature_target30.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization40/Solution40_blm_0/feature_target40.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization50/Solution50_blm_0/feature_target50.rdata")


target <- feature_target10[,c("relative_target","relative_held")]
target <- rbind(target, feature_target20[,c("relative_target","relative_held")])
target <- rbind(target, feature_target30[,c("relative_target","relative_held")])
target <- rbind(target, feature_target40[,c("relative_target","relative_held")])
target <- rbind(target, feature_target50[,c("relative_target","relative_held")])


A <- ggplot(target, aes(x=relative_held))+
  geom_bar(stat="bin", aes(fill=factor(relative_target)))+
  scale_x_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5))+
  xlab("Protection observed")+
  ylab("Number of biodiversity metrics")+
  theme_bw()+
  theme(panel.grid.major = element_blank())+
  guides(fill=guide_legend(title="Protection target"))


load("05_Marine_Spatial_Planning/03_Prioritization/Second_round/Prioritization30/Solution30_blm_10/feature_target30.rdata")

B <- ggplot(feature_target30, aes(x=relative_held))+
  geom_bar(stat="bin", fill="#00BF7D")+
  scale_x_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5))+
  xlab("Protection observed")+
  ylab("Number of biodiversity metrics")+
  theme_bw()+
  theme(panel.grid.major = element_blank())+
  guides(fill=guide_legend(title="Protection target"))

ggarrange(A, B, ncol=2, labels = c("A", "B"), common.legend = T, legend = "bottom")
ggsave("05_Marine_Spatial_Planning/03_Prioritization/feature_targets.png", width = 8, height = 5)
