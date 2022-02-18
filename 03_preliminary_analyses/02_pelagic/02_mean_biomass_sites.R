library(tidyverse)
library(ggplot2)

load("02_formating_data/02_pelagic/Rdata/acoustic_pelagic.rdata")
load("00_metadata/acoustic_explanatory_variables.rdata")

acoustic_site <- cbind(acoustic_pelagic[, -c(1,2)], acoustic_var[,"Site"])
names(acoustic_site)[names(acoustic_site)=="acoustic_var[, \"Site\"]"] <- "Site"
acoustic_site$Site <- as.character(acoustic_site$Site)

acoustic_site <- acoustic_site %>%
  filter(Site != "Transit")

list_site <- split(acoustic_site, acoustic_site$Site)

sites <- unique(acoustic_site$Site)

means <- vector("list", 15)
means <- lapply(means, function(x){
  x <- data.frame(site=character(79), mean=numeric(79), sd=numeric(79), depth=character(79))
})

for (i in 1:length(list_site)) {
  for (j in 1:79) {
    means[[i]][j,"mean"] <- mean(list_site[[i]][,j], na.rm=T)
    means[[i]][j,"sd"] <- sd(list_site[[i]][,j], na.rm=T)
  }
  means[[i]]$site <- names(list_site)[[i]]
  means[[i]]$depth <- colnames(list_site[[i]][,c(1:79)])
  
}              


mean_site <- bind_rows(means)
mean_site$depth <- sub(".*_", "", mean_site$depth) 
mean_site$depth <- as.numeric(mean_site$depth)


facet_site <- ggplot(mean_site)+
  geom_ribbon(aes(ymax=mean+sd, ymin=mean-sd, x=depth), fill = "lightblue", alpha = 0.8)+
  geom_line(aes(x=depth, y=mean), size=1)+
  coord_flip()+
  scale_x_reverse()+
  ylab("mean biomass")+
  xlab("depth")+
  theme_bw()+
  facet_wrap(~site)

ggsave(facet_site, file="03_preliminary_analyses/02_pelagic/plot_mean_biomass_site.png")
