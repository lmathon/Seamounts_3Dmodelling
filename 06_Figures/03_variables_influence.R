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
load("04_Modelling/01_benthic/01_BRUVs/BRT_Output_bruvs/best_gbmStep_reduced_gaussian_brt_Log_biomass.RData")

#plot
data=gbm::plot.gbm(tbmod, "BottomDepth", return.grid=T)

bruvs_biomass <- ggplot(data, aes(x=BottomDepth, y=y))+
  geom_point()+
  geom_smooth(se=FALSE, span=0.3, size=1.5, col="#018571")+
  ylab("log(BRUVS biomass)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


# import model
load("04_Modelling/02_pelagic/01_acoustic/BRT_Output_acoustic/best_gbmStep_reduced_gaussian_brt_log_acoustic.RData")

#plot
data=gbm::plot.gbm(tbmod, "Sampling_Depth", return.grid=T)

pelagic_acoustic <- ggplot(data, aes(x=Sampling_Depth, y=y))+
  geom_point()+
  geom_smooth(se=FALSE, span=0.3, size=1.5, col="#018571")+
  ylab("log(pelagic acoustic biomass)")+
  theme_bw()+
  ggtitle("Depth")+
  theme(plot.title = element_text(size = 12,face="bold",hjust = 0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


# import model
load("04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic/best_gbmStep_reduced_gaussian_brt_logAcousticFond.RData")

#plot
data=gbm::plot.gbm(tbmod, "SSTmean", return.grid=T)

benthic_acoustic_SST <- ggplot(data, aes(x=SSTmean, y=y))+
  geom_point()+
  geom_smooth(se=FALSE, span=0.3, size=1.5, col="#018571")+
  ylab("log(benthic acoustic biomass)")+
  theme_bw()+
  ggtitle("Temperature")+
  theme(plot.title = element_text(size = 12,face="bold",hjust = 0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


#plot
data=gbm::plot.gbm(tbmod, "LandMinDist", return.grid=T)

benthic_acoustic_Dist <- ggplot(data, aes(x=LandMinDist, y=y))+
  geom_point()+
  geom_smooth(se=FALSE, span=0.3, size=1.5, col="#018571")+
  ylab("log(benthic acoustic biomass)")+
  theme_bw()+
  ggtitle("Remoteness")+
  theme(plot.title = element_text(size = 12,face="bold",hjust = 0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


# import model
load("04_Modelling/01_benthic/02_eDNA/BRT_Output_edna/best_gbmStep_reduced_gaussian_brt_richness.RData")

#plot
data=gbm::plot.gbm(tbmod, "TravelTime", return.grid=T)

benthic_edna_Dist <- ggplot(data, aes(x=TravelTime, y=y))+
  geom_point()+
  geom_smooth(se=FALSE, span=0.3, size=1.5, col="#018571")+
  ylab("benthic MOTU richness")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


#plot
data=gbm::plot.gbm(tbmod, "SSTmax", return.grid=T)

benthic_edna_SST <- ggplot(data, aes(x=SSTmax, y=y))+
  geom_point()+
  geom_smooth(se=FALSE, span=0.3, size=1.5, col="#018571")+
  ylab("benthic MOTU richness")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill=NA))


##################################
## plot all together

Figure4 <- ggarrange(pelagic_acoustic, benthic_acoustic_SST, benthic_acoustic_Dist, bruvs_biomass, benthic_edna_SST, benthic_edna_Dist,
          nrow=2, ncol=3, labels=c("A", "B", "C", "D", "E", "F"))

ggsave(Figure4, filename = "06_Figures/Figure4.png")
