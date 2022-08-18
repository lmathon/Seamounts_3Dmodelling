library(tidyverse)
library(vegan)
library(ecodist)
library(ade4)
library(ggplot2)
library(ape)
library(cluster)


# load species/abundance matrix
load("02_formating_data/01_Benthic/Rdata/bruvs_species_matrix.rdata")

# transform into presence-absence
bruvs_pa <- as.data.frame(t(bruvs_species))
bruvs_pa[bruvs_pa>1] <- 1


# Jaccard distance 

dist_species <- vegdist(bruvs_pa, method = "jaccard") 

    # dendrogram
clust_species <- agnes(dist_species, method = "ward")

clust_species <- as.hclust(clust_species)

colors = c("red", "blue", "green", "black", "orange", "yellow")
clus4 = cutree(clust_species, 4)

pdf("03_preliminary_analyses/03_cluster_assemblages/cluster_jaccard_4.pdf", width = 4, height = 12)
plot(as.phylo(clust_species), tip.color = colors[clus4], cex = 0.4)
dev.off()



    # PCoA
pcoa_species <- dudi.pco(dist_species)

data2plot <- pcoa_species$li
data2plot$col <- clus4

ggplot(data2plot, aes(x=A1, y=A2))+
  geom_point(aes(x=A1, y=A2), col=colors[data2plot$col])+
  xlab("PCoA1")+
  ylab("PCoA2")+
  theme(legend.position = c(0.18,0.87),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title.y = element_text(size=11),
        axis.title.x = element_text(size=11),
        panel.border = element_rect(fill = NA))


ggsave(filename = "03_preliminary_analyses/03_cluster_assemblages/pcoa_jaccard_4.png")


# Bray distance on abundance 

bruvs_abund <- as.data.frame(t(bruvs_species))
bruvs_abund <- log1p(bruvs_abund)

dist_species <- vegdist(bruvs_abund, method = "bray") 

    # dendrogram
clust_species <- agnes(sqrt(dist_species), method = "ward")

clust_species <- as.hclust(clust_species)

colors = c("red", "blue", "green", "black", "orange", "yellow")
clus4 = cutree(clust_species, 4)

pdf("03_preliminary_analyses/03_cluster_assemblages/cluster_bray_4.pdf", width = 4, height = 12)
plot(as.phylo(clust_species), tip.color = colors[clus4], cex = 0.4)
dev.off()



    # PCoA
pcoa_species <- dudi.pco(dist_species)

data2plot <- pcoa_species$li
data2plot$col <- clus4

ggplot(data2plot, aes(x=A1, y=A2))+
  geom_point(aes(x=A1, y=A2), col=colors[data2plot$col])+
  xlab("PCoA1")+
  ylab("PCoA2")+
  theme(legend.position = c(0.18,0.87),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title.y = element_text(size=11),
        axis.title.x = element_text(size=11),
        panel.border = element_rect(fill = NA))


ggsave(filename = "03_preliminary_analyses/03_cluster_assemblages/pcoa_bray_4.png")

