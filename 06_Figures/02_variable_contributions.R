library(tidyverse)
library(ggplot2)
library(funk)
library(reshape2)

# load model outputs
load("04_Modelling/01_benthic/01_BRUVs/BRT_Output_bruvs/best_gbmStep_reduced_gaussian_brt_Log_biomass.RData")
BRT_biomass_bruvs <- tbmod$contributions

load("04_Modelling/01_benthic/01_BRUVs/BRT_Output_bruvs/best_gbmStep_reduced_gaussian_brt_richness_tot.RData")
BRT_richness_bruvs <- tbmod$contributions

load("04_Modelling/01_benthic/02_eDNA/BRT_Output_edna/best_gbmStep_reduced_gaussian_brt_richness.RData")
BRT_richness_edna_b <- tbmod$contributions

load("04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic/best_gbmStep_reduced_gaussian_brt_logAcousticFond.RData")
BRT_acoutic_biomass_b <- tbmod$contributions

load("04_Modelling/02_pelagic/01_acoustic/BRT_Output_acoustic/best_gbmStep_reduced_gaussian_brt_log_acoustic.RData")
BRT_acoutic_biomass_p <- tbmod$contributions

load("04_Modelling/02_pelagic/02_eDNA/BRT_Output_edna/best_gbmStep_reduced_gaussian_brt_log_richness.RData")
BRT_richness_edna_p <- tbmod$contributions

load("04_Modelling/01_benthic/01_BRUVs/GJAM_Output_bruvs/gjam_model.rdata")
GJAM_species_abund <- gjam[["parameters"]][["sensTable"]]
GJAM_species_abund <- GJAM_species_abund[-14,]

load("04_Modelling/01_benthic/02_eDNA/GJAM_Output_edna/gjam_model.rdata")
GJAM_MOTU_b <- gjam[["parameters"]][["sensTable"]]
GJAM_MOTU_b <- GJAM_MOTU_b[-14,]

load("04_Modelling/02_pelagic/02_eDNA/GJAM_Output_edna/gjam_model.rdata")
GJAM_MOTU_p <- gjam[["parameters"]][["sensTable"]]
GJAM_MOTU_p <- GJAM_MOTU_p[-15,]

############################################
# sum variables contrib per group and model

var_contrib <- as.data.frame(matrix(nrow=10, ncol=1))
names(var_contrib) <- "variable"
var_contrib$variable <- c("SST", "Depth", "Distance", "CurrentVelocity", "Seafloor_Temp", "Salinity",
                          "Summit_Rugosity", "Suspended_Particulate_Matter", "Chlorophyll_A", "Habitat")

var_contrib$BRUVS_biomass <- c(BRT_biomass_bruvs["SSTmax","rel.inf"],
                               BRT_biomass_bruvs["BottomDepth","rel.inf"],
                               BRT_biomass_bruvs["TravelTime","rel.inf"],
                               0,
                               BRT_biomass_bruvs["seafloorTemp","rel.inf"],
                               BRT_biomass_bruvs["Salinity","rel.inf"],
                               0,
                               BRT_biomass_bruvs["SuspendedParticulateMatter","rel.inf"],
                               0,
                               0)

var_contrib$BRUVS_richness <- c(BRT_richness_bruvs["SSTmean","rel.inf"],
                                BRT_richness_bruvs["BottomDepth","rel.inf"],
                                0,
                                0,
                                0,
                                BRT_richness_bruvs["Salinity","rel.inf"],
                                BRT_richness_bruvs["SummitRugosity","rel.inf"],
                                0,
                                0,
                                0)

var_contrib$eDNA_richness_benthic <- c(BRT_richness_edna_b["SSTmax","rel.inf"],
                                       0,
                                       BRT_richness_edna_b["TravelTime","rel.inf"],
                                       0,
                                       0,
                                       BRT_richness_edna_b["Salinity","rel.inf"],
                                       0,
                                       0,
                                       BRT_richness_edna_b["Chla","rel.inf"],
                                       0)

var_contrib$acoustic_biomass_benthic <- c(BRT_acoutic_biomass_b["SSTmean","rel.inf"],
                                          BRT_acoutic_biomass_b["BottomDepth","rel.inf"],
                                          BRT_acoutic_biomass_b["LandMinDist","rel.inf"],
                                          BRT_acoutic_biomass_b["EastwardVelocity","rel.inf"]+BRT_acoutic_biomass_b["NorthwardVelocity","rel.inf"],
                                          0,
                                          0,
                                          0,
                                          0,
                                          BRT_acoutic_biomass_b["Chla","rel.inf"],
                                          0)


var_contrib$acoustic_biomass_pelagic <- c(BRT_acoutic_biomass_p["SSTmean","rel.inf"],
                                          BRT_acoutic_biomass_p["Sampling_Depth","rel.inf"],
                                          BRT_acoutic_biomass_p["TravelTime","rel.inf"],
                                          0,
                                          0,
                                          0,
                                          0,
                                          0,
                                          0,
                                          0)

var_contrib$eDNA_richness_pelagic <- c(0,
                                       BRT_richness_edna_p["Sampling_Depth","rel.inf"],
                                       BRT_richness_edna_p["LandMinDist","rel.inf"],
                                       BRT_richness_edna_p["EastwardVelocity","rel.inf"]+BRT_richness_edna_p["NorthwardVelocity","rel.inf"],
                                       BRT_richness_edna_p["seafloorTemp","rel.inf"],
                                       BRT_richness_edna_p["Salinity","rel.inf"],
                                       BRT_richness_edna_p["SummitRugosity","rel.inf"],
                                       0,
                                       0,
                                       0)


var_contrib$species_abundance <- c(((GJAM_species_abund["SSTmax","Estimate"]+GJAM_species_abund["I(SSTmax^3)","Estimate"])*100)/sum(GJAM_species_abund$Estimate),
                                    ((GJAM_species_abund["BottomDepth","Estimate"]+GJAM_species_abund["I(BottomDepth^2)","Estimate"])*100)/sum(GJAM_species_abund$Estimate),
                                    ((GJAM_species_abund["ReefMinDist","Estimate"]+GJAM_species_abund["I(ReefMinDist^2)","Estimate"])*100)/sum(GJAM_species_abund$Estimate),
                                    ((GJAM_species_abund["EastwardVelocity","Estimate"]+GJAM_species_abund["NorthwardVelocity","Estimate"]+GJAM_species_abund["I(EastwardVelocity^3)","Estimate"])*100)/sum(GJAM_species_abund$Estimate),
                                    0,
                                    ((GJAM_species_abund["Salinity","Estimate"]+GJAM_species_abund["I(Salinity^2)","Estimate"])*100)/sum(GJAM_species_abund$Estimate),
                                    0,
                                    0,
                                    (GJAM_species_abund["Chla","Estimate"]*100)/sum(GJAM_species_abund$Estimate),
                                    (GJAM_species_abund["HabitatDeepSlope","Estimate"]*100)/sum(GJAM_species_abund$Estimate))


var_contrib$benthic_MOTU_reads <- c(((GJAM_MOTU_b["SSTmean","Estimate"]+GJAM_MOTU_b["I(SSTmean^2)","Estimate"])*100)/sum(GJAM_MOTU_b$Estimate),
                                    ((GJAM_MOTU_b["BottomDepth","Estimate"]+GJAM_MOTU_b["I(BottomDepth^2)","Estimate"])*100)/sum(GJAM_MOTU_b$Estimate),
                                    0,
                                    ((GJAM_MOTU_b["NorthwardVelocity","Estimate"]+GJAM_MOTU_b["I(NorthwardVelocity^3)","Estimate"])*100)/sum(GJAM_MOTU_b$Estimate),
                                    0,
                                    ((GJAM_MOTU_b["Salinity","Estimate"]+GJAM_MOTU_b["I(Salinity^3)","Estimate"])*100)/sum(GJAM_MOTU_b$Estimate),
                                    ((GJAM_MOTU_b["SummitRugosity","Estimate"]+GJAM_MOTU_b["I(SummitRugosity^2)","Estimate"])*100)/sum(GJAM_MOTU_b$Estimate),
                                    ((GJAM_MOTU_b["SuspendedParticulateMatter","Estimate"]+GJAM_MOTU_b["I(SuspendedParticulateMatter^2)","Estimate"])*100)/sum(GJAM_MOTU_b$Estimate),
                                    0,
                                    (GJAM_MOTU_b["HabitatDeepSlope","Estimate"]*100)/sum(GJAM_MOTU_b$Estimate))


var_contrib$pelagic_MOTU_reads <- c((GJAM_MOTU_p["SSTmax","Estimate"]*100)/sum(GJAM_MOTU_p$Estimate),
                                    (GJAM_MOTU_p["SamplingDepth","Estimate"]*100)/sum(GJAM_MOTU_p$Estimate),
                                    (GJAM_MOTU_p["ReefMinDist","Estimate"]*100)/sum(GJAM_MOTU_p$Estimate),
                                    ((GJAM_MOTU_p["NorthwardVelocity","Estimate"]+GJAM_MOTU_p["I(NorthwardVelocity^2)","Estimate"])*100)/sum(GJAM_MOTU_p$Estimate),
                                    ((GJAM_MOTU_p["seafloorTemp","Estimate"]+GJAM_MOTU_p["I(seafloorTemp^3)","Estimate"])*100)/sum(GJAM_MOTU_p$Estimate),
                                    ((GJAM_MOTU_p["Salinity","Estimate"]+GJAM_MOTU_p["I(Salinity^2)","Estimate"])*100)/sum(GJAM_MOTU_p$Estimate),
                                    ((GJAM_MOTU_p["SummitRugosity","Estimate"]+GJAM_MOTU_p["I(SummitRugosity^2)","Estimate"])*100)/sum(GJAM_MOTU_p$Estimate),
                                    (GJAM_MOTU_p["SuspendedParticulateMatter","Estimate"]*100)/sum(GJAM_MOTU_p$Estimate),
                                    (GJAM_MOTU_p["Chla","Estimate"]*100)/sum(GJAM_MOTU_p$Estimate),
                                    (GJAM_MOTU_p["HabitatDeepSlope","Estimate"]*100)/sum(GJAM_MOTU_p$Estimate))


save(var_contrib, file="06_Figures/Rdata/var_contrib.rdata")


################################################
# plot relative contrib

var_contrib2 <- melt(var_contrib, id="variable")
names(var_contrib2) <- c("explanatory_variable", "response_variable", "contribution")
var_contrib2$explanatory_variable2 <- factor(var_contrib2$explanatory_variable, levels = c("Habitat", "Chlorophyll_A", "Suspended_Particulate_Matter", "CurrentVelocity", "Seafloor_Temp", "Summit_Rugosity", "Salinity", "SST", "Distance", "Depth"))
var_contrib2$response_variable <- gsub("_", " ", var_contrib2$response_variable)

var_contrib_facet <- ggplot(data = var_contrib2, 
       aes(x = explanatory_variable2, y = contribution)) +
  #geom_hline(aes(yintercept = 0), colour = "black") + 
  geom_bar(stat = "identity", width = 0.2) +
  coord_flip() + 
  theme_sleek(base_size = 24) + 
  facet_grid(~ response_variable, scales = "free_y",labeller = label_wrap_gen(width=10)) + 
  scale_y_continuous(breaks=seq(0, 100, 50), limits=c(0, 100))+ 
  ylab("Relative contribution (%)") +
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 10),
        strip.text.x = element_text(size=9,face="bold"),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9))



var_contrib$mean_contrib <- rowMeans(var_contrib[,2:9])
for (i in 1:nrow(var_contrib)) {
  var_contrib[i,"sd_contrib"] <- sd(var_contrib[i,2:9])
}
var_contrib$variable2 <- factor(var_contrib$variable, levels = c("Habitat", "Chlorophyll_A", "Suspended_Particulate_Matter", "CurrentVelocity", "Seafloor_Temp", "Summit_Rugosity", "Salinity", "SST", "Distance", "Depth"))

mean_contrib <- ggplot(data = var_contrib, 
       aes(x = variable2, y = mean_contrib)) +
  geom_errorbar(aes(ymin = ifelse(mean_contrib-sd_contrib < 0, 0, mean_contrib-sd_contrib), ymax = mean_contrib+sd_contrib), 
                colour = "black", size = 0.5, width = 0) +
  geom_point(size = 2, col="#018571") +
  scale_y_continuous(breaks=seq(0, 100, 50), limits=c(0, 100))+
  coord_flip() + 
  ggtitle("  \nAll \nmodels")+
  theme_sleek(base_size = 24) + 
  ylab("   ") +
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size=9.5,face="bold",hjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin=unit(c(0.2,0.2,0.2,0), 'cm'))
  


plot_var_contrib <- ggarrange(var_contrib_facet,mean_contrib, ncol=2, widths = c(11, 1.2))
ggsave(plot_var_contrib, filename = "06_Figures/Figure3.png")
