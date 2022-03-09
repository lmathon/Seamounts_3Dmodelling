library(fda)
library(fields)
library(FactoMineR)
library(tidyverse)
library(ggpubr)


###################################################################################################################################
# profiles day

load("02_formating_data/02_pelagic/Rdata/acoustic_day.rdata")

coord <- acoustic_day[,c(1,2)]
coord$profil <- c(paste0("P", seq(1:nrow(coord))))
full_data <- acoustic_day[,3:ncol(acoustic_day)] 
profils <- as.data.frame(apply(full_data, 1, rev)) 
colnames(profils) <- c(paste0("P", seq(1:ncol(profils))))


depth <- seq(10,790,by=10)
depth <- -rev(depth)

# echograms
image.plot(1:nrow(t(profils)), depth, t(profils),
           xlab="Profil", ylab ="Depth (m)", las = 1)

# profil
matplot(profils, depth, type = 'l', col = 'grey', las = 1, xlab = "Biomass", ylab = "Depth")


mean_biomass <- rowMeans(t(profils), na.rm = T)
points(mean_biomass, depth, type = 'b', col = "black", pch=16)


# select profils up to 600m and remove profiles with NA
profils_600 <- as.data.frame(apply(profils, 2, rev)) 
profils_600 <- profils_600[c(1:60),]
profils_600 <- profils_600[ ,colSums(is.na(profils_600)) == 0]


depth <- -seq(10,600,by=10)

# profil
matplot(profils_600, depth, type = 'l', col = 'grey', las = 1, xlab = "Biomass", ylab = "Depth")


mean_biomass <- rowMeans(profils_600, na.rm = T)
points(mean_biomass, depth, type = 'b', col = "black", pch=16)


# echograms
image.plot(1:nrow(t(profils_600)), depth, t(profils_600),
           xlab="Profil", ylab ="Depth (m)", las = 1)



# ACP 
pca_day <- PCA(profils_600, scale.unit = T)
summary(pca_day)

kc <- kmeans(pca_day$var$coord, 5)
plot(pca_day$var$coord[,1:2],col=factor(kc$cluster))

clusters <- as.data.frame(kc$cluster)
clusters$profils <- rownames(clusters)
colnames(clusters) <- c("clusters", "profils")


profils_cluster <- vector("list", 5)
for (i in 1:5) {
  cl <- clusters %>% filter(clusters==i)
  cl$profils <- as.factor(cl$profils)
  pr <- as.character(unique(cl$profils))
  profils_cluster[[i]] <- profils_600[, c(pr)]
  profils_cluster[[i]]$mean <- rowMeans(profils_cluster[[i]], na.rm = T)
  profils_cluster[[i]]$sd <- apply(profils_cluster[[i]], 1, sd)
  profils_cluster[[i]]$depth <- -seq(10,600,by=10)
}

plot <- vector("list",5)
for (i in 1:5) {
  df <- profils_cluster[[i]]
  plot[[i]] <- ggplot(df) +
    geom_line(aes(depth, mean))+
    geom_ribbon(aes(x=depth, ymin=mean-sd, ymax=mean+sd), fill='navy', alpha=.2)+
    theme_bw() +
    coord_flip()+
    labs(title=paste0("cluster",i), y="Mean biomass")+
    theme(axis.line = element_line(colour = "black"),
          legend.position = "right",             # position in top left corner
          legend.title = element_blank(),
          legend.text = element_text(size=9),
          panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1))
}

plot_cluster_day <- ggarrange(plotlist = plot, nrow=1, ncol=5)
ggsave(plot_cluster_day, file="03_preliminary_analyses/02_pelagic/plot_cluster_day.png")


kc_depth <- kmeans(pca_day$ind$coord, 4)
plot(pca_day$ind$coord[,1:2],col=factor(kc_depth$cluster))

clusters_depth <- as.data.frame(kc_depth$cluster)
clusters_depth$depth <- rownames(clusters_depth)
colnames(clusters_depth) <- c("clusters", "depth")

###################################################################################################################################
# profiles night

load("02_formating_data/02_pelagic/Rdata/acoustic_night.rdata")


coord <- acoustic_night[,c(1,2)]
coord$profil <- c(paste0("P", seq(1:nrow(coord))))
full_data <- acoustic_night[,3:ncol(acoustic_night)] 
profils <- as.data.frame(apply(full_data, 1, rev)) 
colnames(profils) <- c(paste0("P", seq(1:ncol(profils))))


depth <- seq(10,790,by=10)
depth <- -rev(depth)

# echograms
image.plot(1:nrow(t(profils)), depth, t(profils),
           xlab="Profil", ylab ="Depth (m)", las = 1)

# profil
matplot(profils, depth, type = 'l', col = 'grey', las = 1, xlab = "Biomass", ylab = "Depth")


mean_biomass <- rowMeans(t(profils), na.rm = T)
points(mean_biomass, depth, type = 'b', col = "black", pch=16)


# select profils up to 600m and remove profiles with NA
profils_600 <- as.data.frame(apply(profils, 2, rev)) 
profils_600 <- profils_600[c(1:60),]
profils_600 <- profils_600[ ,colSums(is.na(profils_600)) == 0]


depth <- -seq(10,600,by=10)

# profil
matplot(profils_600, depth, type = 'l', col = 'grey', las = 1, xlab = "Biomass", ylab = "Depth")


mean_biomass <- rowMeans(profils_600, na.rm = T)
points(mean_biomass, depth, type = 'b', col = "black", pch=16)


# echograms
image.plot(1:nrow(t(profils_600)), depth, t(profils_600),
           xlab="Profil", ylab ="Depth (m)", las = 1)


# ACP 
pca_night <- PCA(profils_600, scale.unit = T)
summary(pca_night)

kc <- kmeans(pca_night$var$coord, 4)
plot(pca_night$var$coord[,1:2],col=factor(kc$cluster))

clusters <- as.data.frame(kc$cluster)
clusters$profils <- rownames(clusters)
colnames(clusters) <- c("clusters", "profils")


profils_cluster <- vector("list", 4)
for (i in 1:4) {
  cl <- clusters %>% filter(clusters==i)
  cl$profils <- as.factor(cl$profils)
  pr <- as.character(unique(cl$profils))
  profils_cluster[[i]] <- profils_600[, c(pr)]
  profils_cluster[[i]]$mean <- rowMeans(profils_cluster[[i]], na.rm = T)
  profils_cluster[[i]]$sd <- apply(profils_cluster[[i]], 1, sd)
  profils_cluster[[i]]$depth <- -seq(10,600,by=10)
}

plot <- vector("list",4)
for (i in 1:4) {
  df <- profils_cluster[[i]]
  plot[[i]] <- ggplot(df) +
    geom_line(aes(depth, mean))+
    geom_ribbon(aes(x=depth, ymin=mean-sd, ymax=mean+sd), fill='navy', alpha=.2)+
    theme_bw() +
    coord_flip()+
    labs(title=paste0("cluster",i), y="Mean biomass")+
    theme(axis.line = element_line(colour = "black"),
          legend.position = "right",             # position in top left corner
          legend.title = element_blank(),
          legend.text = element_text(size=9),
          panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1))
}

plot_cluster_night <- ggarrange(plotlist = plot, nrow=1, ncol=4)
ggsave(plot_cluster_night, file="03_preliminary_analyses/02_pelagic/plot_cluster_night.png")


kc_depth <- kmeans(pca_night$ind$coord, 4)
plot(pca_night$ind$coord[,1:2],col=factor(kc_depth$cluster))

clusters_depth <- as.data.frame(kc_depth$cluster)
clusters_depth$depth <- rownames(clusters_depth)
colnames(clusters_depth) <- c("clusters", "depth")


# -----------------------------------------------------------------------------------------------------------------------------------
# Test Bsplines



depth <- seq(10,600,by=10)
knots  <- c(seq(10,600,by=20), 600)
norder <- 5 

nbasis <- length(knots) + norder - 2
rng <- c(min(depth),max(depth))
basis <- create.bspline.basis(rng, nbasis, norder, knots)

# define the roughness penalty
Lfdobj <- 3 # use the second derivative to define the roughness penalty
lambda <- 1e-2
fdPar <- fdPar(basis, Lfdobj, lambda)
profils_600 <- as.matrix(profils_600)

mfd <- smooth.basis(depth, profils_600, fdPar)$fd
dim(test)
par(mfrow=c(1,1))
plot(depth,profils_600[,1],xlab='depth',ylab='biomass',col=2,cex.lab=1.5,cex.axis=1.5)
lines(mfd[1],col=4,lwd=2) # estimated function without considering the monotone contraint


## fonctionne pas
monmfd1 = smooth.monotone(depth,profils_600[,1],fdPar)
Wfdobj = monmfd1$Wfdobj
beta = monmfd1$beta

tfine = seq(10,300,len=100)
vals1 = eval.monfd(tfine,Wfdobj)
# plot the function when considering the monotone contraint
lines(tfine,beta[1] + beta[2]*vals1,col=6,lwd=2)

