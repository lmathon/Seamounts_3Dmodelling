library(ggplot2)
library(tidyverse)

load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization10/Solution10_blm_0/sol10_surface.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization20/Solution20_blm_0/sol20_surface.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization30/Solution30_blm_0/sol30_surface.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization40/Solution40_blm_0/sol40_surface.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization50/Solution50_blm_0/sol50_surface.rdata")

surface_tot <- 85247

table_surface <- data.frame(target=c(10,20,30,40,50))
table_surface$surface <- c(sol10_surface, sol20_surface, sol_surface, sol40_surface, sol50_surface)
table_surface$perc_surface <- (table_surface$surface*100)/surface_tot


ggplot(data=table_surface, aes(x=target, y=perc_surface))+
  geom_point(size=2, col=c("black", "black", "orange", "black", "black"))+
  geom_hline(yintercept=30, linetype="dashed")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60), limits = c(0,60))+
  scale_x_continuous(breaks = c(0,10,20,30,40,50), limits = c(0,50))+
  xlab("Biodiversity target (%)")+
  ylab("Spatial domain protected (%)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(filename = "06_Figures/Figure5.png", width = 4, height=3)
