library(ggplot2)
library(tidyverse)
library(car)

load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization10/Solution10_blm_0/sol10_surface.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization20/Solution20_blm_0/sol20_surface.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization30/Solution30_blm_0/sol30_surface.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization40/Solution40_blm_0/sol40_surface.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization50/Solution50_blm_0/sol50_surface.rdata")

surface_tot <- 85247

table_surface <- data.frame(target=c(10,20,30,40,50))
table_surface$surface <- c(sol10_surface, sol20_surface, sol30_surface, sol40_surface, sol50_surface)
table_surface$perc_surface <- (table_surface$surface*100)/surface_tot


myreg=lm(table_surface$perc_surface~table_surface$target)
summary(myreg)

linearHypothesis(myreg, "table_surface$target = 1")
linearHypothesis(myreg, "(Intercept) = 0")

ggplot(data=table_surface, aes(x=target, y=perc_surface))+
  geom_point(size=2, col=c("black", "black", "orange", "black", "black"))+
  geom_hline(yintercept=30, linetype="dashed", linewidth=0.3)+
  geom_abline(linetype="dashed", color="darkred")+
  geom_abline(slope = 1.07, intercept = 2.84)+
  annotate(geom="text", x=35, y=25, label="R²=1, P<0.001", hjust=0, size=4, color="black", fontface = "bold")+
  annotate(geom="text", x=35, y=20, label="Slope=1.07 (against 1: P<0.001)", hjust=0, size=4, color="black")+
  annotate(geom="text", x=35, y=15, label="Intercept=2.84 (against 0: P<0.001)", hjust=0, size=4, color="black")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60), limits = c(0,60))+
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60), limits = c(0,60))+
  xlab("Biodiversity target (%)")+
  ylab("Spatial domain protected (%)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(filename = "05_Marine_Spatial_Planning/03_Prioritization//Surface_target.png", width = 7, height=5)
