library(fda)
library(fields)

#########################################################################################################################################
## Profiles all data
load("02_formating_data/02_pelagic/Rdata/acoustic_pelagic.rdata")


lat <- acoustic_pelagic[,1]
lon <- acoustic_pelagic[,2]
full_data <- acoustic_pelagic[,3:ncol(acoustic_pelagic)] 
profils <- t(apply(full_data, 1, rev)) 

depth <- seq(10,790,by=10)
depth <-    -rev(depth)

# echograms
image.plot(1:nrow(profils), depth, profils,
           xlab="Profil", ylab ="Depth (m)", las = 1)

# profil
matplot(t(profils), depth, type = 'l', col = 'grey', las = 1, xlab = "Biomass", ylab = "Depth")


mean_biomass <- rowMeans(t(profils), na.rm = T)
points(mean_biomass, depth, type = 'b', col = "black", pch=16)

# select profils up to 600m and remove profiles with NA
profils_600 <- apply(profils, 1, rev)
profils_600 <- profils_600[c(1:60),]
profils_600 <- profils_600[ , colSums(is.na(profils_600)) == 0]
profils_600 <- as.matrix(profils_600)


depth <- seq(10,600,by=10)
depth <-    -rev(depth)
profils_600 <- apply(profils_600, 2, rev)
# profil
matplot(profils_600, depth, type = 'l', col = 'grey', las = 1, xlab = "Biomass", ylab = "Depth")


mean_biomass <- rowMeans(profils_600, na.rm = T)
points(mean_biomass, depth, type = 'b', col = "black", pch=16)


# echograms
image.plot(1:nrow(t(profils_600)), depth, t(profils_600),
           xlab="Profil", ylab ="Depth (m)", las = 1)


###################################################################################################################################
# profiles day

load("02_formating_data/02_pelagic/Rdata/acoustic_day.rdata")


lat <- acoustic_day[,1]
lon <- acoustic_day[,2]
full_data <- acoustic_day[,3:ncol(acoustic_day)] 
profils <- t(apply(full_data, 1, rev)) 

depth <- seq(10,790,by=10)
depth <-    -rev(depth)

# echograms
image.plot(1:nrow(profils), depth, profils,
           xlab="Profil", ylab ="Depth (m)", las = 1)

# profil
matplot(t(profils), depth, type = 'l', col = 'grey', las = 1, xlab = "Biomass", ylab = "Depth")


mean_biomass <- rowMeans(t(profils), na.rm = T)
points(mean_biomass, depth, type = 'b', col = "black", pch=16)

# select profils up to 600m and remove profiles with NA
profils_600 <- apply(profils, 1, rev)
profils_600 <- profils_600[c(1:60),]
profils_600 <- profils_600[ , colSums(is.na(profils_600)) == 0]
profils_600 <- as.matrix(profils_600)


depth <- seq(10,600,by=10)
depth <-    -rev(depth)
profils_600 <- apply(profils_600, 2, rev)
# profil
matplot(profils_600, depth, type = 'l', col = 'grey', las = 1, xlab = "Biomass", ylab = "Depth")


mean_biomass <- rowMeans(profils_600, na.rm = T)
points(mean_biomass, depth, type = 'b', col = "black", pch=16)


# echograms
image.plot(1:nrow(t(profils_600)), depth, t(profils_600),
           xlab="Profil", ylab ="Depth (m)", las = 1)


###################################################################################################################################
# profiles night

load("02_formating_data/02_pelagic/Rdata/acoustic_night.rdata")


lat <- acoustic_night[,1]
lon <- acoustic_night[,2]
full_data <- acoustic_night[,3:ncol(acoustic_night)] 
profils <- t(apply(full_data, 1, rev)) 

depth <- seq(10,790,by=10)
depth <-    -rev(depth)

# echograms
image.plot(1:nrow(profils), depth, profils,
           xlab="Profil", ylab ="Depth (m)", las = 1)

# profil
matplot(t(profils), depth, type = 'l', col = 'grey', las = 1, xlab = "Biomass", ylab = "Depth")


mean_biomass <- rowMeans(t(profils), na.rm = T)
points(mean_biomass, depth, type = 'b', col = "black", pch=16)

# select profils up to 600m and remove profiles with NA
profils_600 <- apply(profils, 1, rev)
profils_600 <- profils_600[c(1:60),]
profils_600 <- profils_600[ , colSums(is.na(profils_600)) == 0]
profils_600 <- as.matrix(profils_600)


depth <- seq(10,600,by=10)
depth <-    -rev(depth)
profils_600 <- apply(profils_600, 2, rev)
# profil
matplot(profils_600, depth, type = 'l', col = 'grey', las = 1, xlab = "Biomass", ylab = "Depth")


mean_biomass <- rowMeans(profils_600, na.rm = T)
points(mean_biomass, depth, type = 'b', col = "black", pch=16)


# echograms
image.plot(1:nrow(t(profils_600)), depth, t(profils_600),
           xlab="Profil", ylab ="Depth (m)", las = 1)



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

mfd <- smooth.basis(depth, profils_600, fdPar)$fd
dim(test)
par(mfrow=c(1,1))
plot(depth,profils_600[,1],xlab='depth',ylab='biomass',col=2,cex.lab=1.5,cex.axis=1.5)
lines(mfd[1],col=4,lwd=2) # estimated function without considering the monotone contraint


monmfd1 = smooth.monotone(depth,test[,1],fdPar)
Wfdobj = monmfd1$Wfdobj
beta = monmfd1$beta

tfine = seq(10,300,len=100)
vals1 = eval.monfd(tfine,Wfdobj)
# plot the function when considering the monotone contraint
lines(tfine,beta[1] + beta[2]*vals1,col=6,lwd=2)

