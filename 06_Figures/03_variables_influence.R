#load libraries

library(ggplot2)
library(jtools)
library(ggstance)
library(rms)
library(fitdistrplus)
library(huxtable)
library(foreach)
library(dplyr)
library(ggpubr)

# import model
load("04_Modelling/01_benthic/01_BRUVs/01_BRT_richness_BRUVS/BRT_Outputs/best_gbmStep_reduced_poisson_brt_richness_tot.RData")

#plot
data=gbm::plot.gbm(tbmod, "SummitDepth", return.grid=T)

bruvs_richness <- ggplot(data, aes(x=SummitDepth, y=y))+
  geom_point()+
  geom_smooth(se=FALSE, span=0.3, size=1.5, col="#018571")+
  ylab("BRUVS richness")+
  xlab("Summit depth (m)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

# import model
load("04_Modelling/01_benthic/01_BRUVs/02_BRT_abundance_BRUVS/BRT_Outputs/best_gbmStep_reduced_gaussian_brt_Log_abundance.RData")

#plot
data=gbm::plot.gbm(tbmod, "BottomDepth", return.grid=T)

bruvs_abundance <- ggplot(data, aes(x=BottomDepth, y=y))+
  geom_point()+
  geom_smooth(se=FALSE, span=0.3, size=1.5, col="#018571")+
  ylab("log(BRUVS abundance)")+
  xlab("Seafloor depth (m)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


# import model
load("04_Modelling/01_benthic/01_BRUVs/03_BRT_biomass_BRUVS/BRT_Outputs/best_gbmStep_reduced_gaussian_brt_Log_biomass.RData")

#plot
data=gbm::plot.gbm(tbmod, "BottomDepth", return.grid=T)

bruvs_biomass <- ggplot(data, aes(x=BottomDepth, y=y))+
  geom_point()+
  geom_smooth(se=FALSE, span=0.3, size=1.5, col="#018571")+
  ylab("log(BRUVS biomass)")+
  xlab("Seafloor depth (m)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))

# import model
load("04_Modelling/01_benthic/02_eDNA/BRT_Output_edna/best_gbmStep_reduced_poisson_brt_richness.RData")

#plot
data=gbm::plot.gbm(tbmod, "TravelTime", return.grid=T)

benthic_edna <- ggplot(data, aes(x=TravelTime, y=y))+
  geom_point()+
  geom_smooth(se=FALSE, span=0.3, size=1.5, col="#018571")+
  ylab("benthic MOTU richness")+
  xlab("Travel time (hour)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


# import model
load("04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic/best_gbmStep_reduced_gaussian_brt_logAcousticFond.RData")

#plot
data=gbm::plot.gbm(tbmod, "SSTmean", return.grid=T)

benthic_acoustic <- ggplot(data, aes(x=SSTmean, y=y))+
  geom_point()+
  geom_smooth(se=FALSE, span=0.3, size=1.5, col="#018571")+
  ylab("log(benthic acoustic biomass)")+
  xlab("Mean SST (°C)")+
  theme_bw()+
  theme(plot.title = element_text(size = 12,face="bold",hjust = 0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))



# import model
load("04_Modelling/02_pelagic/01_acoustic/BRT_Output_acoustic/best_gbmStep_reduced_gaussian_brt_log_acoustic.RData")

#plot
data=gbm::plot.gbm(tbmod, "Sampling_Depth", return.grid=T)

pelagic_acoustic <- ggplot(data, aes(x=Sampling_Depth, y=y))+
  geom_point()+
  geom_smooth(se=FALSE, span=0.3, size=1.5, col="#018571")+
  ylab("log(pelagic acoustic biomass)")+
  xlab("Sampling depth (m)")+
  theme_bw()+
  theme(plot.title = element_text(size = 12,face="bold",hjust = 0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))




##################################
## plot all together

Figure3 <- ggarrange(bruvs_richness, bruvs_abundance, bruvs_biomass, benthic_edna, benthic_acoustic, pelagic_acoustic, 
          nrow=2, ncol=3, labels=c("A", "B", "C", "D", "E", "F"))

ggsave(Figure3, filename = "06_Figures/Figure3.png")
