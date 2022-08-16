library(tidyverse)
library(ggplot2)
library(ggpubr)

load(("02_formating_data/01_Benthic/Rdata/bruvs_abundance_all.rdata"))
load(("02_formating_data/01_Benthic/Rdata/bruvs_richness_all.rdata"))
load(("02_formating_data/01_Benthic/Rdata/bruvs_biomass_all.rdata"))
load(("02_formating_data/01_Benthic/Rdata/edna_richness_benthic.rdata"))

colnames(bruvs_richness_all) <- c("Richness")
bruvs_biomass_all$Station <- rownames(bruvs_biomass_all)
bruvs_richness_all$Station <- rownames(bruvs_richness_all)


bruvs <- full_join(bruvs_abundance_all, bruvs_biomass_all)
bruvs <- full_join(bruvs, bruvs_richness_all)

all <- left_join(bruvs, edna_richness_benthic)

# correlation eDNA richness / BRUVS richness

cor.test(all$Richness, all$richness_tot, method = "spearman")

lm1 <- lm(all$Richness~all$richness_tot)
summary(lm1)
plot1 <- ggplot(all)+
  geom_point(aes(x=richness_tot, y=Richness))+
  xlab("MOTU richness (eDNA)")+
  ylab("species richness (BRUVS)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA),
        axis.title.y = element_text(size=9),
        axis.title.x = element_text(size=9))

# correlation BRUVS abundance / BRUVS richness

cor.test(all$Richness, all$abundance_tot, method = "spearman")

lm2 <- lm(all$Richness~all$abundance_tot)
summary(lm2)
plot2 <- ggplot(all)+
  geom_point(aes(x=abundance_tot, y=Richness))+
  xlab("abundance (BRUVS)")+
  ylab("species richness (BRUVS)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA),
        axis.title.y = element_text(size=9),
        axis.title.x = element_text(size=9))


# correlation BRUVS biomass / BRUVS richness

cor.test(all$Richness, all$biomass_tot, method = "spearman")

lm3 <- lm(all$Richness~all$biomass_tot)
summary(lm3)
plot3 <- ggplot(all)+
  geom_point(aes(x=biomass_tot, y=Richness))+
  xlab("biomass (BRUVS)")+
  ylab("species richness (BRUVS)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA),
        axis.title.y = element_text(size=9),
        axis.title.x = element_text(size=9))

# correlation BRUVS biomass / BRUVS abundance

cor.test(all$abundance_tot, all$biomass_tot, method = "spearman")

lm4 <- lm(all$abundance_tot~all$biomass_tot)
summary(lm4)
plot4 <- ggplot(all)+
  geom_point(aes(x=biomass_tot, y=abundance_tot))+
  xlab("biomass (BRUVS)")+
  ylab("abundance (BRUVS)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA),
        axis.title.y = element_text(size=9),
        axis.title.x = element_text(size=9))


plot_cor <- ggarrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2, labels= c("A", "B", "C", "D"))
ggsave(plot_cor, file="06_Figures/metric_correlation.png")
