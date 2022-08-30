library(tidyverse)
library(ggplot2)
library(funk)
library(reshape2)
library(ggpubr)

# load model outputs
load("04_Modelling/01_benthic/01_BRUVs/01_BRT_richness_BRUVS/BRT_Outputs/best_gbmStep_reduced_poisson_brt_richness_tot.RData")
BRT_richness_bruvs <- tbmod$contributions

load("04_Modelling/01_benthic/01_BRUVs/02_BRT_abundance_BRUVS/BRT_Outputs/best_gbmStep_reduced_gaussian_brt_Log_abundance.RData")
BRT_abundance_bruvs <- tbmod$contributions

load("04_Modelling/01_benthic/01_BRUVs/03_BRT_biomass_BRUVS/BRT_Outputs/best_gbmStep_reduced_gaussian_brt_Log_biomass.RData")
BRT_biomass_bruvs <- tbmod$contributions

load("04_Modelling/01_benthic/02_eDNA/01_BRT_richness_eDNA/BRT_Output_edna/best_gbmStep_reduced_poisson_brt_richness.RData")
BRT_richness_edna_b <- tbmod$contributions

load("04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic/best_gbmStep_reduced_gaussian_brt_logAcousticFond.RData")
BRT_acoustic_biomass_b <- tbmod$contributions

load("04_Modelling/02_pelagic/02_eDNA/01_BRT_richness_eDNA/BRT_Output_edna/best_gbmStep_reduced_gaussian_brt_log_richness.RData")
BRT_richness_edna_p <- tbmod$contributions

load("04_Modelling/02_pelagic/01_acoustic/BRT_Output_acoustic/best_gbmStep_reduced_gaussian_brt_log_acoustic.RData")
BRT_acoustic_biomass_p <- tbmod$contributions

load("04_Modelling/01_benthic/01_BRUVs/04_GJAM_abundance_BRUVS/GJAM_Output_bruvs/gjam_model.rdata")
GJAM_species_abund <- gjam[["parameters"]][["sensTable"]]
GJAM_species_abund$variable <- rownames(GJAM_species_abund)
GJAM_species_abund <- GJAM_species_abund[-14,]
GJAM_species_abund$variable <- gsub("HabitatDeepSlope", "Habitat", GJAM_species_abund$variable)
GJAM_species_abund$group <- c(1,1,2,2,3,3,4,4,5,5,6,7,8)

total <- GJAM_species_abund %>%
  dplyr::select(group, Estimate) %>%
  group_by(group) %>%
  summarise_all(funs(sum)) 
names(total) <- c("group", "total")
GJAM_species_abund <- left_join(total, GJAM_species_abund[,c("group", "variable")])
GJAM_species_abund <- GJAM_species_abund %>% distinct(group, .keep_all=T)
rownames(GJAM_species_abund) <- GJAM_species_abund$variable
GJAM_species_abund <- as.data.frame(GJAM_species_abund)


load("04_Modelling/01_benthic/02_eDNA/02_GJAM_eDNA/GJAM_Output_edna/gjam_model.rdata")
GJAM_MOTU_b <- gjam[["parameters"]][["sensTable"]]
GJAM_MOTU_b$variable <- rownames(GJAM_MOTU_b)
GJAM_MOTU_b <- GJAM_MOTU_b[-16,]
GJAM_MOTU_b$variable <- gsub("HabitatDeepSlope", "Habitat", GJAM_MOTU_b$variable)
GJAM_MOTU_b$group <- c(1,1,2,3,3,4,5,4,6,6,7,7,8,8,9)

total <- GJAM_MOTU_b %>%
  dplyr::select(group, Estimate) %>%
  group_by(group) %>%
  summarise_all(funs(sum)) 
names(total) <- c("group", "total")
GJAM_MOTU_b <- left_join(total, GJAM_MOTU_b[,c("group", "variable")])
GJAM_MOTU_b <- GJAM_MOTU_b %>% distinct(group, .keep_all=T)
rownames(GJAM_MOTU_b) <- GJAM_MOTU_b$variable
GJAM_MOTU_b <- as.data.frame(GJAM_MOTU_b)


load("04_Modelling/02_pelagic/02_eDNA/02_GJAM_eDNA/GJAM_Output_edna/gjam_model.rdata")
GJAM_MOTU_p <- gjam[["parameters"]][["sensTable"]]
GJAM_MOTU_p$variable <- rownames(GJAM_MOTU_p)
GJAM_MOTU_p <- GJAM_MOTU_p[-18,]
GJAM_MOTU_p$variable <- gsub("HabitatDeepSlope", "Habitat", GJAM_MOTU_p$variable)
GJAM_MOTU_p$group <- c(1,1,2,3,4,4,5,6,7,7,8,8,9,10,11,12,13)

total <- GJAM_MOTU_p %>%
  dplyr::select(group, Estimate) %>%
  group_by(group) %>%
  summarise_all(funs(sum)) 
names(total) <- c("group", "total")
GJAM_MOTU_p <- left_join(total, GJAM_MOTU_p[,c("group", "variable")])
GJAM_MOTU_p <- GJAM_MOTU_p %>% distinct(group, .keep_all=T)
rownames(GJAM_MOTU_p) <- GJAM_MOTU_p$variable
GJAM_MOTU_p <- as.data.frame(GJAM_MOTU_p)

############################################
# sum variables contrib per group and model

var_contrib <- as.data.frame(matrix(nrow=15, ncol=1))
names(var_contrib) <- "variable"
var_contrib$variable <- c("Summit_Depth", "Seafloor_Depth", "Sampling_Depth", "Travel_Time", "Distance_Land", 
                          "Distance_Reef", "SST", "CurrentVelocity", "Seafloor_Temp", "Salinity",
                          "Summit_Rugosity", "Summit_Area" ,"Suspended_Particulate_Matter", "Chlorophyll_A", "Habitat")

var_contrib$BRUVS_richness_benthic <- c(BRT_richness_bruvs["SummitDepth","rel.inf"],
                                        BRT_richness_bruvs["BottomDepth","rel.inf"],
                                        0,
                                        BRT_richness_bruvs["TravelTime","rel.inf"],
                                        0,
                                        BRT_richness_bruvs["ReefMinDist","rel.inf"],
                                        0,
                                        0,
                                        0,
                                        0,
                                        BRT_richness_bruvs["SummitRugosity","rel.inf"],
                                        0,
                                        0,
                                        0,
                                        0)

var_contrib$BRUVS_abundance_benthic <- c(0,
                                         BRT_abundance_bruvs["BottomDepth","rel.inf"],
                                         0,
                                         0,
                                         0,
                                         0,
                                         BRT_abundance_bruvs["SSTmax","rel.inf"],
                                         BRT_abundance_bruvs["EastwardVelocity","rel.inf"],
                                         0,
                                         0,
                                         0,
                                         0,
                                         0,
                                         0,
                                         0)

var_contrib$BRUVS_biomass_benthic <- c(BRT_biomass_bruvs["SummitDepth","rel.inf"],
                                       BRT_biomass_bruvs["BottomDepth","rel.inf"],
                                       0,
                                       BRT_biomass_bruvs["TravelTime","rel.inf"],
                                       0,
                                       0,
                                       0,
                                       0,
                                       BRT_biomass_bruvs["seafloorTemp","rel.inf"],
                                       BRT_biomass_bruvs["Salinity","rel.inf"],
                                       0,
                                       0,
                                       0,
                                       0,
                                       0)


var_contrib$eDNA_richness_benthic <- c(0,
                                       0,
                                       0,
                                       BRT_richness_edna_b["TravelTime","rel.inf"],
                                       BRT_richness_edna_b["LandMinDist","rel.inf"],
                                       BRT_richness_edna_b["ReefMinDist","rel.inf"],
                                       BRT_richness_edna_b["SSTmax","rel.inf"],
                                       0,
                                       0,
                                       BRT_richness_edna_b["Salinity","rel.inf"],
                                       0,
                                       0,
                                       0,
                                       BRT_richness_edna_b["Chla","rel.inf"],
                                       0)

var_contrib$acoustic_biomass_benthic <- c(0,
                                          BRT_acoustic_biomass_b["BottomDepth","rel.inf"],
                                          0,
                                          BRT_acoustic_biomass_b["TravelTime","rel.inf"],
                                          BRT_acoustic_biomass_b["LandMinDist","rel.inf"],
                                          BRT_acoustic_biomass_b["ReefMinDist","rel.inf"],
                                          BRT_acoustic_biomass_b["SSTmax","rel.inf"] + BRT_acoustic_biomass_b["SSTmean","rel.inf"], 
                                          0,
                                          0,
                                          0,
                                          0,
                                          0,
                                          BRT_acoustic_biomass_b["SuspendedParticulateMatter","rel.inf"],
                                          0,
                                          0)


var_contrib$eDNA_richness_pelagic <- c(BRT_richness_edna_p["SummitDepth","rel.inf"],
                                       0,
                                       0,
                                       0,
                                       BRT_richness_edna_p["LandMinDist","rel.inf"],
                                       0,
                                       BRT_richness_edna_p["SSTmean","rel.inf"],
                                       BRT_richness_edna_p["NorthwardVelocity","rel.inf"],
                                       BRT_richness_edna_p["seafloorTemp","rel.inf"],
                                       BRT_richness_edna_p["Salinity","rel.inf"],
                                       0,
                                       BRT_richness_edna_p["SummitAreaKm2","rel.inf"],
                                       0,
                                       0,
                                       0)


var_contrib$acoustic_biomass_pelagic <- c(0,
                                          0,
                                          BRT_acoustic_biomass_p["Sampling_Depth","rel.inf"],
                                          BRT_acoustic_biomass_p["TravelTime","rel.inf"],
                                          0,
                                          0,
                                          BRT_acoustic_biomass_p["SSTmean","rel.inf"], 
                                          0,
                                          0,
                                          0,
                                          0,
                                          0,
                                          0,
                                          0,
                                          0)


var_contrib$BRUVS_species_abundance <- c(0,
                                   (GJAM_species_abund["BottomDepth","total"]*100)/sum(GJAM_species_abund$total),
                                   0,
                                   (GJAM_species_abund["TravelTime","total"]*100)/sum(GJAM_species_abund$total),
                                   (GJAM_species_abund["LandMinDist","total"]*100)/sum(GJAM_species_abund$total),
                                   0,
                                   (GJAM_species_abund["SSTmean","total"]*100)/sum(GJAM_species_abund$total), 
                                   (GJAM_species_abund["EastwardVelocity","total"]*100)/sum(GJAM_species_abund$total),
                                   0,
                                   (GJAM_species_abund["Salinity","total"]*100)/sum(GJAM_species_abund$total),
                                   (GJAM_species_abund["SummitRugosity","total"]*100)/sum(GJAM_species_abund$total),
                                   0,
                                   0,
                                   0,
                                   (GJAM_species_abund["Habitat","total"]*100)/sum(GJAM_species_abund$total))



var_contrib$eDNA_MOTU_reads_benthic <- c(0,
                                    (GJAM_MOTU_b["BottomDepth","total"]*100)/sum(GJAM_MOTU_b$total),
                                    0,
                                    (GJAM_MOTU_b["TravelTime","total"]*100)/sum(GJAM_MOTU_b$total),
                                    0,
                                    (GJAM_MOTU_b["ReefMinDist","total"]*100)/sum(GJAM_MOTU_b$total),
                                    (GJAM_MOTU_b["SSTmean","total"]*100)/sum(GJAM_MOTU_b$total), 
                                    (GJAM_MOTU_b["NorthwardVelocity","total"]*100)/sum(GJAM_MOTU_b$total),
                                    0,
                                    (GJAM_MOTU_b["Salinity","total"]*100)/sum(GJAM_MOTU_b$total),
                                    (GJAM_MOTU_b["SummitRugosity","total"]*100)/sum(GJAM_MOTU_b$total),
                                    0,
                                    (GJAM_MOTU_b["SuspendedParticulateMatter","total"]*100)/sum(GJAM_MOTU_b$total),
                                    0,
                                    (GJAM_MOTU_b["Habitat","total"]*100)/sum(GJAM_MOTU_b$total))


var_contrib$eDNA_MOTU_reads_pelagic <- c(0,
                                    (GJAM_MOTU_p["BottomDepth","total"]*100)/sum(GJAM_MOTU_p$total),
                                    (GJAM_MOTU_p["SamplingDepth","total"]*100)/sum(GJAM_MOTU_p$total),
                                    (GJAM_MOTU_p["TravelTime","total"]*100)/sum(GJAM_MOTU_p$total),
                                    0,
                                    (GJAM_MOTU_p["ReefMinDist","total"]*100)/sum(GJAM_MOTU_p$total),
                                    (GJAM_MOTU_p["SSTmean","total"]*100)/sum(GJAM_MOTU_p$total), 
                                    ((GJAM_MOTU_p["NorthwardVelocity","total"]+GJAM_MOTU_p["EastwardVelocity","total"])*100)/sum(GJAM_MOTU_p$total),
                                    (GJAM_MOTU_p["seafloorTemp","total"]*100)/sum(GJAM_MOTU_p$total),
                                    (GJAM_MOTU_p["Salinity","total"]*100)/sum(GJAM_MOTU_p$total),
                                    (GJAM_MOTU_p["SummitRugosity","total"]*100)/sum(GJAM_MOTU_p$total),
                                    (GJAM_MOTU_p["SummitAreaKm2","total"]*100)/sum(GJAM_MOTU_p$total),
                                    (GJAM_MOTU_p["SuspendedParticulateMatter","total"]*100)/sum(GJAM_MOTU_p$total),
                                    0,
                                    (GJAM_MOTU_p["Habitat","total"]*100)/sum(GJAM_MOTU_p$total))


save(var_contrib, file="06_Figures/Rdata/var_contrib.rdata")


################################################
# plot relative contrib

var_contrib2 <- melt(var_contrib, id="variable")
names(var_contrib2) <- c("explanatory_variable", "response_variable", "contribution")
var_contrib2$explanatory_variable2 <- factor(var_contrib2$explanatory_variable, levels = c("Habitat" ,"Chlorophyll_A", "Suspended_Particulate_Matter", "Summit_Area", "Summit_Rugosity", "CurrentVelocity", "Seafloor_Temp",  "Salinity", "SST", "Distance_Reef", "Distance_Land", "Travel_Time", "Sampling_Depth", "Summit_Depth", "Seafloor_Depth"))
var_contrib2$response_variable <- gsub("_", " ", var_contrib2$response_variable)

var_contrib_facet <- ggplot(data = var_contrib2, 
                            aes(x = explanatory_variable2, y = contribution)) +
  #geom_hline(aes(yintercept = 0), colour = "black") + 
  geom_bar(stat = "identity", width = 0.2) +
  coord_flip() + 
  theme_sleek(base_size = 24) + 
  facet_grid(~ response_variable, scales = "free_y",labeller = label_wrap_gen(width=10)) + 
  scale_y_continuous(breaks=c(0, 50), limits=c(0, 100))+ 
  ylab("Relative contribution (%)") +
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 10),
        strip.text.x = element_text(size=9,face="bold"),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9))



var_contrib$Mean_all_models <- rowMeans(var_contrib[,2:11])
for (i in 1:nrow(var_contrib)) {
  var_contrib[i,"sd_contrib"] <- sd(var_contrib[i,2:11])
}
var_contrib$variable2 <- factor(var_contrib$variable, levels = c("Habitat","Chlorophyll_A", "Suspended_Particulate_Matter", "Summit_Area", "Summit_Rugosity", "CurrentVelocity", "Seafloor_Temp",  "Salinity", "SST", "Distance_Reef", "Distance_Land", "Travel_Time", "Sampling_Depth", "Summit_Depth", "Seafloor_Depth"))

mean_contrib <- ggplot(data = var_contrib, 
                       aes(x = variable2, y = Mean_all_models)) +
  geom_errorbar(aes(ymin = ifelse(Mean_all_models-sd_contrib < 0, 0, Mean_all_models-sd_contrib), ymax = Mean_all_models+sd_contrib), 
                colour = "black", size = 0.5, width = 0) +
  geom_point(size = 2, col="#018571") +
  scale_y_continuous(breaks=c(0, 50, 100), limits=c(0, 100))+
  coord_flip() + 
  ggtitle("Mean \nall \nmodels")+
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



plot_var_contrib <- ggarrange(var_contrib_facet,mean_contrib, ncol=2, widths = c(11, 1))
ggsave(plot_var_contrib, filename = "06_Figures/Figure3.png")
