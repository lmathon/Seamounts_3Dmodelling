library(ggplot2)
library(ggpubr)
library(ggrepel)


load("05_Marine_Spatial_Planning/03_Prioritization/Rdata/result_plot.rdata")
load("05_Marine_Spatial_Planning/03_Prioritization/Rdata/result_plot2.rdata")

blm_rank <- ggarrange(result_plot, result_plot2, ncol=2, labels = c("A", "B"))

ggsave(blm_rank, filename = "06_Figures/plot_BLM_rank.png")
