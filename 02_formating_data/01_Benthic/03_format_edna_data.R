library(tidyverse)

load("01_Raw_data/Clean_eDNA/Rdata/02-clean-data.Rdata")
var <- read.csv("01_Raw_data/eDNA_Environmental_Variables_SEAMOUNTS_November2021.csv", sep=";")

var <- var %>%
  filter(Habitat != "Pelagic")

edna <- df_all_filters %>%
  filter(habitat_type %in% c("Sommet"))

edna_motus <- edna[,c("code_explo", "definition", "count_reads")]

# Pull PCR replicate
edna_motus <- edna_motus %>%
  group_by(.dots = c("code_explo", "definition")) %>% 
  summarise(n_reads = sum(count_reads)) %>% 
  ungroup()

# One column per MOTU
edna_motus <- pivot_wider(edna_motus, names_from="definition", values_from="n_reads", values_fill=NA)
edna_motus[is.na(edna_motus)] <- 0
names(edna_motus)[names(edna_motus)=="code_explo"] <- "Station"

# complete with stations with 0 MOTUs
edna_motus <- full_join(edna_motus, var[,c("Station", "Site")])
edna_motus <- edna_motus[, -ncol(edna_motus)]
edna_motus[is.na(edna_motus)] <- 0



# Sum of total richness per station
edna_richness_benthic <- edna_motus[,c("Station")]
edna_motus <- edna_motus[,-1]
rownames(edna_motus) <- edna_richness_benthic$Station

edna_pa <- edna_motus
edna_pa[edna_pa>1] <- 1

edna_richness_benthic$richness_tot <- rowSums(edna_pa)




# save rdata
save(edna_motus, file="02_formating_data/01_Benthic/Rdata/edna_motus_matrix_benthic.rdata")
save(edna_richness_benthic, file = "02_formating_data/01_Benthic/Rdata/edna_richness_benthic.rdata")


# check for correlation between read number and motu richness
edna_richness_benthic$read_number <- rowSums(edna_motus)
cor.test(edna_richness_benthic$richness_tot,edna_richness_benthic$read_number)

plot(edna_richness_benthic$richness_tot~edna_richness_benthic$read_number)

lm <- lm(richness_tot~read_number, data=edna_richness_benthic)
summary(lm)

ggplot(data=edna_richness_benthic, aes(x=read_number, y=richness_tot))+
  geom_point(size=2)+
  geom_abline(slope = 6.033e-06, intercept = 8.901, size=0.8)+
  annotate(geom="text", x=3e+06, y=70, label="Adjusted R square = 0.12", hjust=0, size=4, color="black", fontface = "bold")+
  xlab("Number of reads per sample")+
  ylab("MOTU richness per sample")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

#--------------------------------------------------------------------------------------------------------------------------------------
## Explanatory variables
#--------------------------------------------------------------------------------------------------------------------------------------

edna_var <- read.csv("01_Raw_data/EdnaDataForLaetitia.csv", sep=";")
edna_var <- edna_var[,c("Station", "Sampling_depth")]
edna_var <- edna_var %>% distinct(Station, .keep_all=T)

edna_var <- left_join(var, edna_var)

# change summit area and summit rugosity for deepslope
for (i in 1:nrow(edna_var)) {
  if (edna_var[i,"Habitat"]=="DeepSlope"){
    edna_var[i,"ValleyDepth"] <- 3383
    edna_var[i,"SummitDepth"] <- 0
    edna_var[i,"Height"] <- 3383
    edna_var[i,"SummitAreasKm2"] <- 19268.7292212099
    edna_var[i,"SummitRugosity"] <- 16.680511910144
  }
}


#  transform travel time in hours
edna_var$TravelTime=edna_var$TravelTime / 3600

# transform distances in km
edna_var$ReefMinDist=edna_var$ReefMinDist.m/1000
edna_var$LandMinDist=edna_var$LandMinDist.m/1000

# change Habitat variable
edna_var$Habitat2 <- edna_var$Habitat

edna_var$Habitat <- gsub("Summit500", "Seamount", edna_var$Habitat)
edna_var$Habitat <- gsub("Summit250", "Seamount", edna_var$Habitat)
edna_var$Habitat <- gsub("Summit50", "Seamount", edna_var$Habitat)

edna_var <- edna_var[-c(17,18,25)]

colnames(edna_var) <- c("Station","Site","Latitude","Longitude","Habitat","EastwardVelocity","NorthwardVelocity","Salinity",
                        "SuspendedParticulateMatter","SSTmax","SSTmean","SSTmin","SSTsd","seafloorTemp","Chla","TravelTime",
                        "SummitDepth","ValleyDepth","Height", "SummitAreaKm2","SummitRugosity","BottomDepth","ReefMinDist","LandMinDist", "Habitat2")


save(edna_var, file="00_metadata/edna_explanatory_variables_benthic.rdata")

