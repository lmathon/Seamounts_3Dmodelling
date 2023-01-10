# Accumulation curves on genus, family, order on eDNA data


# Lib 
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(conflicted)


# 
'%ni%' <- Negate("%in%")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("filter", "dplyr")

# Functions
source('03_preliminary_analyses/07_Accumulation_curves/00_functions.R')





# ------------------------------------------------------------------------------- # 
#### On eDNA MOTUs ----
# ------------------------------------------------------------------------------- # 

# data
load("01_Raw_data/Clean_eDNA/Rdata/02-clean-data.Rdata")

df_all_filters <- df_all_filters %>%
  filter(habitat_type %in% c("Sommet", "Pelagique"))

# Split by site
list_read_step4 <- split(df_all_filters, df_all_filters$site_name)


# rank_specify
rank_choice = 'sequence'

# accumlation all plots
liste_accumulation <- lapply(list_read_step4, accumulation_curve_df, species_unit = rank_choice)

# Asymptote of all plots 
liste_asymptote <- lapply(list_read_step4, asymptote_mm, species_unit = rank_choice)

# Unlist
df_accumulation <- bind_rows(liste_accumulation, .id = "site_name")
df_asymptote <- bind_rows(liste_asymptote, .id = "site_name")

# Dataset for plot
df_accumulation_all <- left_join(df_accumulation, df_asymptote, by = "site_name") %>%
  group_by(site_name) %>%
  mutate(position_asymptote_y = 0.20 * max(asymptote), 
         position_asymptote_x = max(sites))

# Add All samples
# Add a global saturation curve, i.e. all samples together?
all_accumulation_motu <- accumulation_curve_df(df_all_filters, species_unit = rank_choice) %>%
  mutate(site_name = "All") %>%
  select(site_name, richness, sd, sites)


# Asymptote of all plots 
all_asymptote_motu <- asymptote_mm(df_all_filters, species_unit = rank_choice) %>%
  mutate(site_name = "All") %>%
  select(site_name, asymptote, slope)


# Bind together
df_all_accumulation <- rbind(df_accumulation, all_accumulation_motu)
df_all_asymptote <- rbind(df_asymptote, all_asymptote_motu)

# 
df_join_all <- df_all_accumulation %>%
  left_join(., df_all_asymptote, by = "site_name") %>%
  group_by(site_name) %>%
  mutate(position_asymptote_y = 1.05*asymptote, 
         position_asymptote_x = max(sites),
         position_slope_y = 0.30 * max(asymptote)) 

colnames(df_join_all) <- c("Site", "richness", "sd", "stations", "asymptote", "slope", "position_asymptote_y", "position_asymptote_x", "position_slope_y")

accu_all <- df_join_all %>%
  filter(Site=="All")
  
accu_sites <- df_join_all %>%
  filter(Site !="All")

# Plot with facet
plot_acc_sites <- ggplot(accu_sites) + 
  geom_ribbon(aes(x = stations, ymin = richness-sd, ymax = richness+sd), fill="grey", alpha = 0.7, show.legend = F) +
  geom_line(aes(x = stations, y = richness)) +
  geom_hline(aes(yintercept = asymptote), linetype = "dashed", size = 1) +
  facet_wrap(~Site, scales = "free", ncol=5) +
  ylab("Number of MOTUs") +
  xlab("Samples (filter)") +
  theme_bw() + 
  ggtitle("MOTUs") + 
  geom_text(aes(x = position_asymptote_x, y =position_asymptote_y, hjust = 1, label = paste(round(asymptote, 0), "MOTUs")), col = "black", size=3)
  
plot_acc_sites

ggsave("03_preliminary_analyses/07_Accumulation_curves/accumulation_curves_eDNA_sites.png")

# plot accumulation all
plot_acc_all <- ggplot(accu_all) + 
  geom_ribbon(aes(x = stations, ymin = richness-sd, ymax = richness+sd), fill="grey", alpha = 0.7, show.legend = F) +
  geom_line(aes(x = stations, y = richness)) +
  geom_hline(aes(yintercept = asymptote), linetype = "dashed", size = 1) +
    ylab("Number of MOTUs") +
  xlab("Samples (filter)") +
  theme_bw() + 
  ggtitle("MOTUs") + 
  geom_text(aes(x = position_asymptote_x, y =position_asymptote_y, hjust = 1, label = paste(round(asymptote, 0), "MOTUs")), col = "black", size=3)

plot_acc_all

ggsave("03_preliminary_analyses/07_Accumulation_curves/accumulation_curves_eDNA_all.png")





# ------------------------------------------------------------------------------- # 
#### On BRUVs species ----
# ------------------------------------------------------------------------------- # 
source('03_preliminary_analyses/07_Accumulation_curves/00_functions_bruvs.R')


# data
bruvs <- read.csv("01_Raw_data/BruvsDataForLaetitia.csv", sep=";")

bruvs <- bruvs %>%
  select(Site, Station, Species, SR)

# Split by site
list_species <- split(bruvs, bruvs$Site)


# rank_specify
rank_choice = 'Species'

# accumlation all plots
liste_accumulation <- lapply(list_species, accumulation_curve_df, species_unit = rank_choice)


# Unlist
df_accumulation <- bind_rows(liste_accumulation, .id = "site_name")


# Add All samples
# Add a global saturation curve, i.e. all samples together?
all_accumulation_species <- accumulation_curve_df(bruvs, species_unit = rank_choice) %>%
  mutate(site_name = "All") %>%
  select(site_name, richness, sd, sites)


# Asymptote of all plots 
all_asymptote_species <- asymptote_mm(bruvs, species_unit = rank_choice) %>%
  mutate(site_name = "All") %>%
  select(site_name, asymptote, slope)


# Bind together
df_all_accumulation <- rbind(df_accumulation, all_accumulation_species)


colnames(df_all_accumulation) <- c("Site", "richness", "sd", "stations")

accu_all <- df_all_accumulation %>%
  filter(Site=="All")

accu_sites <- df_all_accumulation %>%
  filter(Site !="All")

# Plot with facet
plot_acc_sites <- ggplot(accu_sites) + 
  geom_ribbon(aes(x = stations, ymin = richness-sd, ymax = richness+sd), fill="grey", alpha = 0.7, show.legend = F) +
  geom_line(aes(x = stations, y = richness)) +
  facet_wrap(~Site, scales = "free", ncol=5) +
  ylab("Number of species") +
  xlab("BRUVs") +
  theme_bw() + 
  ggtitle("Species")

plot_acc_sites

ggsave("03_preliminary_analyses/07_Accumulation_curves/accumulation_curves_BRUVs_sites.png")

# plot accumulation all
plot_acc_all <- ggplot(accu_all) + 
  geom_ribbon(aes(x = stations, ymin = richness-sd, ymax = richness+sd), fill="grey", alpha = 0.7, show.legend = F) +
  geom_line(aes(x = stations, y = richness)) +
  ylab("Number of species") +
  xlab("BRUVs") +
  theme_bw() + 
  ggtitle("Species")

plot_acc_all

ggsave("03_preliminary_analyses/07_Accumulation_curves/accumulation_curves_BRUVs_all.png")
